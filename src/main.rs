use esp_idf_svc::hal::delay::FreeRtos;
use esp_idf_svc::hal::gpio::PinDriver;
use esp_idf_svc::hal::peripherals::Peripherals;
use esp_idf_svc::sys::EspError;

fn main() -> Result<(), EspError> {
    // It is necessary to call this function once. Otherwise some patches to the runtime
    // implemented by esp-idf-sys might not link properly. See https://github.com/esp-rs/esp-idf-template/issues/71
    esp_idf_svc::sys::link_patches();

    // Bind the log crate to the ESP Logging facilities
    esp_idf_svc::log::EspLogger::initialize_default();

    log::info!("Hello, world!");

    // grab a pin, make it an output
    let gpio10 = Peripherals::take()?.pins.gpio10;
    let mut pin = PinDriver::output(gpio10)?;

    // twiddle
    loop {
        pin.set_high()?;
        FreeRtos::delay_ms(500);

        pin.set_low()?;
        FreeRtos::delay_ms(500);
    }
}
