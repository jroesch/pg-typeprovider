pub trait Joinable {
    fn join(&mut self, delim: &str) -> String;
}

impl<'a, A : Str, T : Iterator<A> + 'a> Joinable for T  {
    fn join(&mut self, delim: &str) -> String {
        let mut retval = "".to_string();
        let mut prev = self.next();

        if prev.is_none() {
            return retval;
        }

        let mut cur = self.next();

        while cur.is_some() {
            retval.push_str(prev.unwrap().as_slice());
            retval.push_str(delim);
            prev = cur;
            cur = self.next();
        }
                
        retval.push_str(prev.unwrap().as_slice());
        retval
    } // join
}
