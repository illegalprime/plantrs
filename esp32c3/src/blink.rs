use esp_idf_svc::{
    hal::{
        delay::FreeRtos,
        gpio::{Gpio10, PinDriver},
        peripheral::Peripheral,
    },
    sys::EspError,
};

#[allow(dead_code)]
pub fn blink(pin: impl Peripheral<P = Gpio10>) -> Result<(), EspError> {
    // grab a pin, make it an output
    let mut pin = PinDriver::output(pin)?;

    // twiddle
    loop {
        pin.set_high()?;
        FreeRtos::delay_ms(500);

        pin.set_low()?;
        FreeRtos::delay_ms(500);
    }
}
