use futures::channel::mpsc::{unbounded, UnboundedReceiver, UnboundedSender};
use parking_lot::Mutex;
use ruc::*;

type NotificationSinks<T> = Mutex<Vec<UnboundedSender<T>>>;

pub struct Notifications<T> {
    sinks: NotificationSinks<T>,
}

impl<T: Clone> Default for Notifications<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Notifications<T> {
    pub fn new() -> Self {
        Self {
            sinks: Default::default(),
        }
    }

    pub fn notification_sinks(&self) -> &NotificationSinks<T> {
        &self.sinks
    }

    pub fn notification_stream(&self) -> UnboundedReceiver<T> {
        let (sink, stream) = unbounded();
        self.sinks.lock().push(sink);
        stream
    }

    pub fn notify(&self, data: T) -> Result<()> {
        self.sinks
            .lock()
            .retain(|sink| sink.unbounded_send(data.clone()).is_ok());
        Ok(())
    }
}
