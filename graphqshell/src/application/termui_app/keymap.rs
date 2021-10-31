use crate::infra::termui::engine::keys::Key;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("Key is already defined for a different command")]
    DuplicateKey(char),
}

#[repr(transparent)]
#[derive(Clone)]
pub struct KeyMap<T: Clone>(HashMap<char, KeymapEntry<T>>);

#[derive(Clone)]
pub enum KeymapEntry<T: Clone> {
    Command(String, T),
    Group(String, Box<KeyMap<T>>),
}

impl<T: Clone> KeyMap<T> {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn match_key(&self, c: &char) -> Option<&KeymapEntry<T>> {
        self.0.get(&c)
    }

    pub fn bindings(&self) -> Vec<(char, String)> {
        let mut result = Vec::with_capacity(self.0.len());

        for (c, entry) in self.0.iter() {
            let descr = match entry {
                KeymapEntry::Command(label, _) => label.clone(),
                KeymapEntry::Group(label, _) => label.clone(),
            };

            result.push((c.clone(), descr))
        }

        result.sort_by(|l, r| l.0.cmp(&r.0));
        result
    }

    pub fn cmd<S: Into<String>>(&mut self, c: char, label: S, cmd: T) -> Result<&mut Self> {
        match self.0.entry(c) {
            Entry::Occupied(_) => Err(Error::DuplicateKey(c)),
            Entry::Vacant(e) => {
                e.insert(KeymapEntry::Command(label.into(), cmd));
                Ok(self)
            }
        }
    }

    pub fn group<S: Into<String>>(&mut self, c: char, label: S, keymap: Self) -> Result<&mut Self> {
        match self.0.entry(c) {
            Entry::Occupied(_) => Err(Error::DuplicateKey(c)),
            Entry::Vacant(e) => {
                e.insert(KeymapEntry::Group(label.into(), Box::new(keymap)));
                Ok(self)
            }
        }
    }
}
