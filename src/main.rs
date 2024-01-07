use std::sync::mpsc;

use esp_idf_svc::eventloop::EspSystemEventLoop;
use esp_idf_svc::hal::peripherals::Peripherals;
use esp_idf_svc::io::EspIOError;
use esp_idf_svc::mqtt::client::QoS;

use crate::mqtt::{Command, Response};

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

    // main loop
    while let Ok(request) = rx.recv() {
        let body = match request.command {
            Command::Add(a, b) => (a + b).to_string(),
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
