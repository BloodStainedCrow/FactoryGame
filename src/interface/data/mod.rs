// This data is generated from the currently loaded mods and provided using a static.
// Getting the data from the static will need to be wrapped in an abstraction so I do not need to write unsafe everywhere...
// To update the mods I will require unsafe code that asserts that no game is currently running and using the data.

// I want to have this as a static instead of as a ref I pass through everything (like I did previously) since that makes the code SOOO much less verbose

pub static mut DATA: Data = Data;

pub struct Data;
