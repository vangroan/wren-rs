pub fn escape(s: impl AsRef<str>) -> String {
    let mut result = String::new();

    for ch in s.as_ref().chars() {
        let escaped_ch = match ch {
            '\n' => "\\n",
            '\r' => "\\r",
            '\t' => "\\t",
            _ => {
                result.push(ch);
                ""
            }
        };

        result.push_str(escaped_ch);
    }

    result
}
