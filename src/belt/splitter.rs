use std::sync::{
    atomic::{AtomicU8, Ordering},
    mpsc::{sync_channel, Receiver, SyncSender},
    Arc, Weak,
};

use crate::item::Item;

use super::simple::{BeltStorage, SimpleBelt};

#[derive(Debug)]
pub struct Splitter {
    items_in: Receiver<Item>,
    items_in_len: Arc<AtomicU8>,
    items_out: [SyncSender<Item>; 2],
    items_out_len: [Arc<AtomicU8>; 2],
    next_out: bool,
}

impl Splitter {
    pub fn new(belt_in: &mut SimpleBelt, belts_out: &mut [&mut SimpleBelt; 2]) -> Self {
        let (tx, rx) = sync_channel(2);
        let items_in = rx;
        let items_in_len = Arc::new(AtomicU8::new(0));

        let inserter_in = SplitterInserterIn {
            items_in_of_splitter: tx,
            items_in_count: Arc::downgrade(&items_in_len),
        };

        let (tx1, rx1) = sync_channel(2);
        let (tx2, rx2) = sync_channel(2);
        let items_out = [tx1, tx2];
        let items_out_len = [Arc::new(AtomicU8::new(0)), Arc::new(AtomicU8::new(0))];

        let inserter_out_0 = SplitterInserterOut {
            items_out_of_splitter: rx1,
            items_out_count: Arc::downgrade(&items_out_len[0]),
        };

        let inserter_out_1 = SplitterInserterOut {
            items_out_of_splitter: rx2,
            items_out_count: Arc::downgrade(&items_out_len[1]),
        };

        belt_in.add_splitter_inserter_in(inserter_in);

        belts_out[0].add_splitter_inserter_out(inserter_out_0);
        belts_out[1].add_splitter_inserter_out(inserter_out_1);

        Self {
            items_in,
            items_in_len,
            items_out,
            items_out_len,
            next_out: false,
        }
    }

    ///
    /// # Panics
    /// When the channel dies
    pub fn update(&mut self) {
        if self.items_in_len.load(Ordering::Relaxed) > 0 {
            let index = usize::from(self.next_out);

            if self.items_out_len[index].load(Ordering::Relaxed) < 2 {
                let item = self
                    .items_in
                    .recv()
                    .expect("Channel closed for splitter inserter");

                self.items_out[index]
                    .try_send(item)
                    .expect("Channel closed for splitter inserter");

                self.next_out = !self.next_out;

                self.items_in_len.fetch_sub(1, Ordering::Relaxed);
                self.items_out_len[index].fetch_add(1, Ordering::Relaxed);
            } else if self.items_out_len[usize::from(!self.next_out)].load(Ordering::Relaxed) < 2 {
                let item = self
                    .items_in
                    .recv()
                    .expect("Channel closed for splitter inserter");

                self.items_out[usize::from(!self.next_out)]
                    .try_send(item)
                    .expect("Channel closed for splitter inserter");

                self.items_in_len.fetch_sub(1, Ordering::Relaxed);
                self.items_out_len[usize::from(!self.next_out)].fetch_add(1, Ordering::Relaxed);
            }
        }
    }
}

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub(super) struct SplitterInserterIn {
    items_in_of_splitter: SyncSender<Item>,
    items_in_count: Weak<AtomicU8>,
}

impl SplitterInserterIn {
    pub(super) fn update_simple(&mut self, belt_storage: &mut BeltStorage) {
        let in_count = self.items_in_count.upgrade().expect("Splitter dropped");

        if in_count.load(Ordering::Relaxed) < 2 {
            if let Some(item) = belt_storage.get_item_from_pos_and_remove(0) {
                self.items_in_of_splitter
                    .try_send(item)
                    .expect("Channel closed for splitter inserter");

                in_count.fetch_add(1, Ordering::Relaxed);
            }
        }
    }
}

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub(super) struct SplitterInserterOut {
    items_out_of_splitter: Receiver<Item>,
    items_out_count: Weak<AtomicU8>,
}

impl SplitterInserterOut {
    pub(super) fn update_simple(&mut self, belt_storage: &mut BeltStorage) {
        let out_count = self.items_out_count.upgrade().expect("Splitter dropped");

        if out_count.load(Ordering::Relaxed) > 0
            && belt_storage.check_for_space(belt_storage.get_belt_len() - 1)
        {
            if let Ok(item) = self.items_out_of_splitter.try_recv() {
                let res = belt_storage.try_put_item_in_pos(item, belt_storage.get_belt_len() - 1);

                out_count.fetch_sub(1, Ordering::Relaxed);

                assert!(res);
            } else {
                unreachable!()
            }
        }
    }
}
