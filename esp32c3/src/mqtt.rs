use std::{str::from_utf8, sync::mpsc::Sender};

use const_format::formatcp;
use esp_idf_svc::{
    io::EspIOError,
    mqtt::client::{EspMqttClient, Event, LwtConfiguration, MqttClientConfiguration, QoS},
    sys::EspError,
};
use log::warn;
use serde::{Deserialize, Serialize};

const MQTT_PREFIX: &str = "plantrs";
const MQTT_HELLO: &str = formatcp!("{}/hello", MQTT_PREFIX);
const MQTT_GOODBYE: &str = formatcp!("{}/goodbye", MQTT_PREFIX);
const MQTT_DISCOVER: &str = formatcp!("{}/discover", MQTT_PREFIX);

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Command {
    Add(u32, u32),
    Drive(u32),
}

#[derive(Debug, Deserialize)]
pub struct Request {
    pub command: Command,
    pub response_topic: String,
    pub correlate: u128,
}

pub enum Message {
    Request(Request),
    Hello,
}

#[derive(Debug, Serialize)]
pub struct Response {
    pub body: String,
    pub correlate: u128,
}

pub fn mqtt(
    url: &str,
    id: &str,
    tx: Sender<Message>,
) -> Result<Box<EspMqttClient<'static>>, EspIOError> {
    // default configuration
    let mqtt_config = MqttClientConfiguration {
        lwt: Some(LwtConfiguration {
            topic: MQTT_GOODBYE,
            payload: id.as_bytes(),
            qos: QoS::AtLeastOnce,
            retain: false,
        }),
        ..Default::default()
    };
    // forward all received requests to handler thread
    let mut mqtt_client = EspMqttClient::new(url, &mqtt_config, move |m| {
        match m {
            // say hello
            Ok(Event::Received(msg)) if msg.topic() == Some(MQTT_DISCOVER) => {
                tx.send(Message::Hello)
            }
            // handle messages
            Ok(Event::Received(msg)) => match serde_json::from_slice(msg.data()) {
                Ok(request) => tx.send(Message::Request(request)),
                Err(_) => Ok(warn!("could not parse: {:?}", from_utf8(msg.data()))),
            },
            // other server events
            _ => Ok(warn!("Received from MQTT: {:?}", m)),
        }
        .expect("channel disconnect")
    })?;
    // subscribe to requests topic
    let sub_topic = format!("{}/{}/request", MQTT_PREFIX, id);
    mqtt_client.subscribe(&sub_topic, QoS::AtLeastOnce)?;
    // subscribe to discover topic
    mqtt_client.subscribe(MQTT_DISCOVER, QoS::AtLeastOnce)?;
    // say initial hello
    hello(&mut mqtt_client, id)?;
    // return a handle to the client
    Ok(Box::new(mqtt_client))
}

pub fn hello(mqtt_client: &mut EspMqttClient<'static>, id: &str) -> Result<u32, EspError> {
    mqtt_client.publish(MQTT_HELLO, QoS::AtLeastOnce, false, id.as_bytes())
}
