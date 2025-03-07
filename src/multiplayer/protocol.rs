use std::collections::{BTreeMap, BTreeSet};

use bitvec::array::BitArray;

// TODO:
const PACKETS_PER_SECOND: u8 = 60;

type PacketSequence = u16;
type BitfieldType = BitArray<u64>;

fn sequence_newer_than(s1: PacketSequence, s2: PacketSequence) -> bool {
    ((s1 > s2) && (s1 - s2 <= PacketSequence::MAX / 2))
        || ((s1 < s2) && (s2 - s1 > PacketSequence::MAX / 2))
}

struct Connection {
    local_sequence_number: PacketSequence,
    remote_sequence_number: PacketSequence,

    to_send: BTreeMap<PacketSequence, Body>,
    recieved: BTreeSet<PacketSequence>,

    next_packet_dequeued: PacketSequence,
    recieved_body_buffer: BTreeMap<PacketSequence, Body>,
}

impl Connection {
    fn tick(&mut self, to_send: Body) -> Body {
        self.to_send.insert(self.local_sequence_number, to_send);

        let ack_value = self
            .recieved
            .range(
                self.recieved
                    .first()
                    .unwrap_or(&self.remote_sequence_number)..,
            )
            .nth(BitfieldType::ZERO.len())
            .unwrap_or(&self.remote_sequence_number);

        let bitfield = self.recieved.range(..ack_value);

        self.local_sequence_number = self.local_sequence_number.wrapping_add(1);

        todo!()
    }

    fn on_recieve(&mut self, packet: Packet) {
        self.recieved.insert(packet.sequence);

        // Handle acks
        self.to_send.remove(&packet.ack_value);
        for (seq, acked) in (1..=packet.bitfield.len())
            .rev()
            .map(|offs| packet.ack_value.wrapping_sub(offs as u16))
            .zip(packet.bitfield.iter().by_vals())
        {
            if acked {
                self.to_send.remove(&seq);
            }
        }

        if sequence_newer_than(packet.sequence, self.remote_sequence_number) {
            self.remote_sequence_number = packet.sequence;
        }

        self.recieved_body_buffer
            .insert(packet.sequence, packet.body);
    }
}

struct Packet {
    sequence: PacketSequence,
    ack_value: PacketSequence,
    bitfield: BitfieldType,
    body: Body,
}

struct Body {
    actions: Vec<()>,
}
