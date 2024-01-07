use std::sync::mpsc::Sender;

use esp_idf_svc::{
    io::EspIOError,
    mqtt::client::{EspMqttClient, EspMqttMessage, Event, MqttClientConfiguration, QoS},
};
use log::warn;
use serde::{Deserialize, Serialize};

const MQTT_PREFIX: &str = "plantrs";

#[derive(Debug, Deserialize)]
pub enum Command {
    Add(u32, u32),
}

#[derive(Debug, Deserialize)]
pub struct Request {
    pub command: Command,
    pub response_topic: String,
    pub correlate: u128,
}

#[derive(Debug, Serialize)]
pub struct Response {
    pub body: String,
    pub correlate: u128,
}

pub fn mqtt(
    url: &str,
    id: &str,
    tx: Sender<Request>,
) -> Result<Box<EspMqttClient<'static>>, EspIOError> {
    // default configuration
    let mqtt_config = MqttClientConfiguration::default();
    // forward all received requests to handler thread
    let mut mqtt_client = EspMqttClient::new(url, &mqtt_config, move |m| match m {
        Ok(Event::Received(msg)) => send_request(msg, &tx)
            .unwrap_or_else(|e| warn!("Error processing request {:?}: {:?}", msg.topic(), e)),
        _ => warn!("Received from MQTT: {:?}", m),
    })?;
    // subscribe to the topic
    let sub_topic = format!("{}/{}/request", MQTT_PREFIX, id);
    mqtt_client.subscribe(&sub_topic, QoS::AtLeastOnce)?;
    // say hello
    let hello_topic = format!("{}/hello", MQTT_PREFIX);
    mqtt_client.publish(&hello_topic, QoS::AtLeastOnce, false, id.as_bytes())?;
    // return a handle to the client
    Ok(Box::new(mqtt_client))
}

fn send_request(msg: &EspMqttMessage<'_>, tx: &Sender<Request>) -> anyhow::Result<()> {
    // attempt to parse into text and JSON
    log::info!("got request: {:?}", std::str::from_utf8(msg.data()));
    let request = serde_json::from_slice(msg.data())?;
    log::info!("parsed json: {:?}", request);
    // send request to main handler
    tx.send(request)?;
    Ok(())
}
