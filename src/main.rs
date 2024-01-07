use esp_idf_svc::eventloop::EspSystemEventLoop;
use esp_idf_svc::hal::peripherals::Peripherals;
use esp_idf_svc::io::EspIOError;

mod blink;
mod http;
mod wifi;

#[toml_cfg::toml_config]
pub struct Config {
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

    // blink
    blink::blink(peripherals.pins.gpio10)?;

    Ok(())
}
