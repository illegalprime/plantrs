use std::{
    sync::mpsc::{self, RecvTimeoutError},
    time::Duration,
};

use esp_idf_svc::hal::peripherals::Peripherals;
use esp_idf_svc::io::EspIOError;
use esp_idf_svc::mqtt::client::QoS;
use esp_idf_svc::{eventloop::EspSystemEventLoop, hal::gpio::PinDriver};

use crate::mqtt::{Command, Message, Response};

mod blink;
mod http;
mod mqtt;
mod wifi;

#[toml_cfg::toml_config]
pub struct Config {
    #[default("")]
    mqtt_url: &'static str,
    #[default("")]
    mqtt_id: &'static str,
    #[default("")]
    wifi_ssid: &'static str,
    #[default("")]
    wifi_psk: &'static str,
}

fn main() -> Result<(), EspIOError> {
    // It is necessary to call this function once. Otherwise some patches to the runtime
    // implemented by esp-idf-sys might not link properly. See https://github.com/esp-rs/esp-idf-template/issues/71
    esp_idf_svc::sys::link_patches();

    // Bind the log crate to the ESP Logging facilities
    esp_idf_svc::log::EspLogger::initialize_default();

    // greet
    log::info!("Hello, world!");

    let peripherals = Peripherals::take()?;
    let sysloop = EspSystemEventLoop::take()?;

    // connect to wifi
    let _wifi = wifi::wifi(
        CONFIG.wifi_ssid,
        CONFIG.wifi_psk,
        esp_idf_svc::wifi::AuthMethod::WPA2Personal,
        peripherals.modem,
        sysloop,
    )?;

    // HTTP GET example
    http::get("https://crouton.net/")?;

    // simple request-reply handler
    let (tx, rx) = mpsc::channel();

    // MQTT example
    let mut mqtt_client = mqtt::mqtt(CONFIG.mqtt_url, CONFIG.mqtt_id, tx)?;

    // hold a pin for x seconds
    let mut hold_pin = PinDriver::output(peripherals.pins.gpio10)?;
    let mut hold_pin_secs = 0;

    // main loop
    loop {
        let request = match rx.recv_timeout(Duration::from_secs(1)) {
            Ok(Message::Request(r)) => r,
            Ok(Message::Hello) => {
                // say hello
                mqtt::hello(&mut mqtt_client, CONFIG.mqtt_id)?;
                continue;
            }
            Err(RecvTimeoutError::Timeout) => {
                match hold_pin_secs {
                    0 => hold_pin.set_low()?,
                    x => hold_pin_secs = x - 1,
                };
                continue;
            }
            Err(RecvTimeoutError::Disconnected) => break,
        };
        let body = match request.command {
            Command::Add(a, b) => (a + b).to_string(),
            Command::Drive(secs) => {
                hold_pin_secs = secs;
                match hold_pin_secs {
                    0 => hold_pin.set_low()?,
                    _ => hold_pin.set_high()?,
                };
                "ok".into()
            }
        };
        let response = Response {
            body,
            correlate: request.correlate,
        };
        let json = match serde_json::to_string(&response) {
            Ok(j) => j,
            Err(e) => {
                log::warn!("failed to serialize {:?}: {:?}", response, e);
                continue;
            }
        };
        log::info!("sending response: {}", json);
        mqtt_client.publish(
            &request.response_topic,
            QoS::AtLeastOnce,
            false,
            json.as_bytes(),
        )?;
    }

    Ok(())
}
