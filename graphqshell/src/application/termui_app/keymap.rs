use std::collections::HashMap;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Clone, Debug)]
pub enum Error {
    #[error("Key is already defined for a different command")]
    DuplicateKey(char),
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct KeyMap<T: Clone>(HashMap<char, KeymapEntry<T>>);

#[derive(Clone, Debug)]
pub enum KeymapEntry<T: Clone> {
    Command(String, T),
    Group(String, Box<KeyMap<T>>),
}

impl<T: Clone> KeyMap<T> {
    pub fn empty() -> Self {
        Self::builder().build()
    }

    pub fn builder() -> Builder<T> {
        Builder::empty()
    }

    pub fn match_key(&self, c: &char) -> Option<&KeymapEntry<T>> {
        self.0.get(&c)
    }

    pub fn bindings(&self) -> Vec<(char, String)> {
        let mut result = Vec::with_capacity(self.0.len());

        for (c, entry) in self.0.iter() {
            let descr = match entry {
                KeymapEntry::Command(label, _) => format!("+{}", label),
                KeymapEntry::Group(label, _) => label.clone(),
            };

            result.push((c.clone(), descr))
        }

        result.sort_by(|l, r| l.0.cmp(&r.0));
        result
    }
}

pub struct Builder<T: Clone>(HashMap<char, KeymapEntry<T>>);

impl<T: Clone> Builder<T> {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn cmd<S: Into<String>>(&mut self, c: char, label: S, cmd: T) -> &mut Self {
        self.0.insert(c, KeymapEntry::Command(label.into(), cmd));
        self
    }

    pub fn group_map<S: Into<String>>(&mut self, c: char, label: S, map: KeyMap<T>) -> &mut Self {
        self.0.insert(c, KeymapEntry::Group(label.into(), Box::new(map)));
        self
    }

    pub fn group<S: Into<String>, B: FnOnce(&mut Self) -> ()>(&mut self, c: char, label: S, group_builder: B) -> &mut Self {
        let mut builder = Self::empty();
        group_builder(&mut builder);
        self.0.insert(c, KeymapEntry::Group(label.into(), Box::new(builder.build())));
        self
    }

    pub fn build(&self) -> KeyMap<T> {
        KeyMap(self.0.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow;

    #[derive(Clone, PartialEq, Debug)]
    enum Cmd {
        A,
        B,
    }

    #[test]
    fn builds_from_single_command() -> anyhow::Result<()> {
        KeyMap::builder().cmd('a', "label", Cmd::A).build();
        Ok(())
    }

    #[test]
    fn builds_from_group() -> anyhow::Result<()> {
        KeyMap::builder()
            .group('a', "label", |g| {
                g.cmd('b', "label", Cmd::B);
            })
            .build();
        Ok(())
    }

    #[test]
    fn match_cmd() -> anyhow::Result<()> {
        let keymap = KeyMap::builder().cmd('a', "label", Cmd::A).build();

        assert_matches!(keymap.match_key(&'a'), Some(KeymapEntry::Command(_, Cmd::A)));
        Ok(())
    }

    #[test]
    fn match_nothing() -> anyhow::Result<()> {
        let keymap = KeyMap::builder().cmd('a', "label", Cmd::A).build();

        assert_matches!(keymap.match_key(&'b'), None);
        Ok(())
    }

    #[test]
    fn match_group() -> anyhow::Result<()> {
        let keymap = KeyMap::builder()
            .group('a', "label", |g| {
                g.cmd('b', "label", Cmd::B);
            })
            .build();

        if let KeymapEntry::Group(_, inner) = keymap.match_key(&'a').unwrap() {
            assert_matches!(inner.match_key(&'b'), Some(KeymapEntry::Command(_, Cmd::B)));
            Ok(())
        } else {
            Err(anyhow::anyhow!("Expected group"))
        }
    }
}
