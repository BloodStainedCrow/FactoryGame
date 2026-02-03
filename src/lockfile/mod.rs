use std::{fs::File, path::Path};

#[derive(Debug)]
pub(crate) struct LockfileUnique {
    file: std::fs::File,
}

impl LockfileUnique {
    /*
        Blocks until the lockfile was locked exclusively
    */
    pub fn create_blocking(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let file = File::create(path)?;
        file.lock()?;

        Ok(Self { file })
    }

    pub fn release(self) -> std::io::Result<()> {
        self.file.unlock()?;

        Ok(())
    }
}

impl Drop for LockfileUnique {
    fn drop(&mut self) {
        if let Err(e) = self.file.unlock() {
            log::error!("Failed unlocking unique lockfile: {e:?}");
        }
    }
}

#[derive(Debug)]
pub(crate) struct LockfileShared {
    file: std::fs::File,
}

impl LockfileShared {
    /*
        Blocks until the lockfile was locked shared
    */
    pub fn create_blocking(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let file = File::create(path)?;
        file.lock_shared()?;

        Ok(Self { file })
    }
}

impl Drop for LockfileShared {
    fn drop(&mut self) {
        if let Err(e) = self.file.unlock() {
            log::error!("Failed unlocking shared lockfile: {e:?}");
        }
    }
}
