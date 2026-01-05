use std::mem::ManuallyDrop;

use egui_show_info::{EguiDisplayable, InfoExtractor, ShowInfo};
use get_size2::GetSize;

pub(crate) struct SmallCapVec<T> {
    ptr: *mut T,
    len: u32,
    capacity: u32,
}

impl<T: std::fmt::Debug> std::fmt::Debug for SmallCapVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.access(|v| v.fmt(f))
    }
}

impl<T: Clone> Clone for SmallCapVec<T> {
    fn clone(&self) -> Self {
        self.access(|v| v.clone()).try_into().unwrap()
    }
}

unsafe impl<T: Send> Send for SmallCapVec<T> {}
// Send might be overkill here but I dont need it so better safe than sorry
unsafe impl<T: Send + Sync> Sync for SmallCapVec<T> {}

impl<T> Drop for SmallCapVec<T> {
    fn drop(&mut self) {
        let dropped_vec =
            unsafe { Vec::from_raw_parts(self.ptr, self.len as usize, self.capacity as usize) };
        std::mem::drop(dropped_vec);
    }
}

impl<T> VecHolder<T> for SmallCapVec<T> {
    fn new() -> Self {
        let (ptr, len, capacity) = Vec::new().into_raw_parts();

        assert!(
            u16::try_from(capacity).is_ok(),
            "SmallCapVec capacity too large at creation"
        );

        Self {
            ptr,
            len: len.try_into().unwrap(),
            capacity: capacity.try_into().unwrap(),
        }
    }

    #[inline(always)]
    fn access<R>(&self, action: impl FnOnce(&Vec<T>) -> R) -> R {
        // Wrapping the Vec in ManuallyDrop is needed to avoid a double drop if the user closure panics.
        let temp_vec = ManuallyDrop::new(unsafe {
            Vec::from_raw_parts(self.ptr, self.len as usize, self.capacity as usize)
        });

        let ret = action(&temp_vec);

        assert!(
            u16::try_from(temp_vec.capacity()).is_ok(),
            "SmallCapVec grew too large"
        );

        let (ptr, len, capacity) = ManuallyDrop::into_inner(temp_vec).into_raw_parts();
        assert!(ptr == self.ptr, "pointer changed with shared ref?");
        assert!(len == self.len as usize, "len changed with shared ref?");
        assert!(
            capacity == self.capacity as usize,
            "capacity changed with shared ref?"
        );

        ret
    }

    #[inline(always)]
    fn access_mut<R>(&mut self, action: impl FnOnce(&mut Vec<T>) -> R) -> R {
        // Wrapping the Vec in ManuallyDrop is needed to avoid a double drop if the user closure panics.
        let mut temp_vec = ManuallyDrop::new(unsafe {
            Vec::from_raw_parts(self.ptr, self.len as usize, self.capacity as usize)
        });

        let ret = action(&mut temp_vec);

        assert!(
            u16::try_from(temp_vec.capacity()).is_ok(),
            "SmallCapVec grew too large"
        );

        let (ptr, len, capacity) = ManuallyDrop::into_inner(temp_vec).into_raw_parts();
        self.ptr = ptr;
        self.len = len.try_into().unwrap();
        self.capacity = capacity.try_into().unwrap();

        ret
    }

    fn into_vec(self) -> Vec<T> {
        let Self { ptr, len, capacity } = self;

        unsafe { Vec::from_raw_parts(ptr, len.try_into().unwrap(), capacity.try_into().unwrap()) }
    }
}

impl<T> std::ops::Deref for SmallCapVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { core::slice::from_raw_parts(self.ptr, self.len as usize) }
    }
}

impl<T> std::ops::DerefMut for SmallCapVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { core::slice::from_raw_parts_mut(self.ptr, self.len as usize) }
    }
}

impl<T> TryFrom<Vec<T>> for SmallCapVec<T> {
    type Error = <usize as TryInto<u32>>::Error;

    fn try_from(value: Vec<T>) -> Result<Self, Self::Error> {
        let (ptr, len, capacity) = value.into_raw_parts();

        Ok(Self {
            ptr,
            len: len.try_into()?,
            capacity: capacity.try_into()?,
        })
    }
}

impl<T> VecHolder<T> for Vec<T> {
    fn new() -> Self {
        Vec::new()
    }

    fn access<R>(&self, action: impl FnOnce(&Vec<T>) -> R) -> R {
        action(self)
    }

    fn access_mut<R>(&mut self, action: impl FnOnce(&mut Vec<T>) -> R) -> R {
        action(self)
    }

    fn into_vec(self) -> Vec<T> {
        self
    }
}

impl<T: GetSize> GetSize for SmallCapVec<T> {
    fn get_heap_size(&self) -> usize {
        self.access(|v| v.get_heap_size())
    }
}

impl<T: ShowInfo<E, Info>, E: InfoExtractor<T, Info>, Info: EguiDisplayable> ShowInfo<E, Info>
    for SmallCapVec<T>
where
    E: InfoExtractor<SmallCapVec<T>, Info>,
{
    fn show_fields<C: egui_show_info::Cache<String, Info>>(
        &self,
        extractor: &mut E,
        ui: &mut egui::Ui,
        path: String,
        cache: &mut C,
    ) {
        // TODO:
    }

    fn show_info<C: egui_show_info::Cache<String, Info>>(
        &self,
        extractor: &mut E,
        ui: &mut egui::Ui,
        path: &str,
        cache: &mut C,
    ) {
        let mut path = String::from(path);
        path.push_str(std::any::type_name::<Self>());

        let our_info = if let Some(cached) = cache.get(&path) {
            cached
        } else {
            let new = extractor.extract_info(self);
            cache.put(path.clone(), &new);
            new
        };

        egui::CollapsingHeader::new(std::any::type_name::<Self>())
            .id_salt(&path)
            .show(ui, |ui| {
                our_info.show(ui);
                self.show_fields(extractor, ui, path, cache);
            });
    }
}
impl<T: serde::Serialize> serde::Serialize for SmallCapVec<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.access(|v| v.serialize(serializer))
    }
}
impl<'de, T: serde::Deserialize<'de>> serde::Deserialize<'de> for SmallCapVec<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let v = Vec::<T>::deserialize(deserializer)?;

        // FIXME:
        Ok(v.try_into().expect("SmallCapVec too long"))
    }
}

pub(crate) trait VecHolder<T> {
    fn new() -> Self;
    fn access<R>(&self, action: impl FnOnce(&Vec<T>) -> R) -> R;
    fn access_mut<R>(&mut self, action: impl FnOnce(&mut Vec<T>) -> R) -> R;
    fn into_vec(self) -> Vec<T>;
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    #[should_panic]
    fn panic_during_access() {
        let mut small = SmallCapVec::<u8>::new();

        small.access_mut(|v| {
            v.push(100);
        });

        small.access(|v| {
            assert!(v.is_empty());
        });

        std::mem::drop(small);
    }
}
