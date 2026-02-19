use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU64, Ordering},
};

use parking_lot::Mutex;

#[derive(Debug)]
pub struct ProgressInfo {
    inner: Arc<Inner>,
}

#[derive(Debug)]
struct Inner {
    // NOTE(Tim): This u64 is interpreted as a f64
    progress: AtomicU64,
    stages: Mutex<Vec<ProgressStage>>,
    message_changed: AtomicBool,
}

#[derive(Debug)]
struct ProgressStage {
    message: Option<String>,
    begin: f64,
    multiplier: f64,
}

impl Clone for ProgressInfo {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl ProgressInfo {
    pub fn new() -> Self {
        let zero: f64 = 0.0;
        let zero: AtomicU64 = AtomicU64::new(zero.to_bits());

        Self {
            inner: Arc::new(Inner {
                progress: zero,
                stages: Mutex::new(vec![ProgressStage {
                    message: None,
                    begin: 0.0,
                    multiplier: 1.0,
                }]),
                message_changed: AtomicBool::new(false),
            }),
        }
    }

    pub fn get_progress(&self) -> f64 {
        f64::from_bits(self.inner.progress.load(Ordering::Relaxed))
    }

    pub fn get_message(&self) -> Option<String> {
        if self.inner.message_changed.load(Ordering::SeqCst) {
            self.inner
                .stages
                .lock()
                .iter()
                .rev()
                .find_map(|stage| stage.message.as_ref())
                .cloned()
        } else {
            None
        }
    }

    pub fn set_progress(&self, progress: f64) {
        let binding = self.inner.stages.lock();
        let stage = binding.last().unwrap();

        self.inner.progress.store(
            (stage.begin + progress * stage.multiplier).to_bits(),
            Ordering::Relaxed,
        );
    }

    // FIXME: This is technically a race condition here. But it is prob fine
    pub fn add_progress(&self, amount: f64) {
        let binding = self.inner.stages.lock();
        let stage = binding.last().unwrap();

        let current = self.get_progress();

        self.inner.progress.store(
            (current + amount * stage.multiplier).to_bits(),
            Ordering::Relaxed,
        );
    }

    // FIXME: This is technically a race condition here. But it is prob fine
    pub fn push_stage(&self, perc_of_current_stage: f64, message: Option<String>) {
        let mut stages = self.inner.stages.lock();
        let multiplier =
            stages.iter().map(|stage| stage.multiplier).product::<f64>() * perc_of_current_stage;

        let begin = self.get_progress();
        if message.is_some() {
            self.inner.message_changed.store(true, Ordering::SeqCst);
        }
        stages.push(ProgressStage {
            message,
            begin,
            multiplier,
        });
    }

    // FIXME: This is technically a race condition here. But it is prob fine
    pub fn pop_stage(&self) {
        // When a stage is popped it must be done
        self.set_progress(1.0);
        let mut stages = self.inner.stages.lock();
        let stage = stages.pop().expect("Popped more stages than were pushed");
        if stage.message.is_some() {
            self.inner.message_changed.store(true, Ordering::SeqCst);
        }
    }
}
