use esp_idf_svc::{
    http::{
        client::{Configuration, EspHttpConnection, Request},
        Method,
    },
    io::EspIOError,
};
use log::info;

pub fn get(url: impl AsRef<str>) -> Result<(), EspIOError> {
    let mut connection = EspHttpConnection::new(&Configuration {
        use_global_ca_store: true,
        crt_bundle_attach: Some(esp_idf_svc::sys::esp_crt_bundle_attach),
        ..Default::default()
    })?;

    let headers = [("accept", "text/plain")];

    connection.initiate_request(Method::Get, url.as_ref(), &headers)?;

    let request = Request::wrap(connection);

    let mut response = request.submit()?;

    let status = response.status();

    info!("http status code: {}", status);

    let mut buf = [0; 1024];
    loop {
        let read = response.read(&mut buf)?;
        if read == 0 {
            break;
        }
        let text = std::str::from_utf8(&buf[..read]);
        match text {
            Ok(t) => print!("{}", t),
            Err(_) => info!("could not parse into utf8"),
        }
    }

    Ok(())
}
