#![feature(phase)]
#[phase(plugin)]
extern crate pg_typeprovider;

// TODO: this is very weird; I need to put this in a module, and pg_typeprovider
// must be put in twice.  This might be a bug in Rust; ask Jared.
mod testing {
    extern crate core;
    extern crate time;
    extern crate postgres;
    extern crate pg_typeprovider;

    use self::time::{now, Timespec};

    use self::postgres::{Connection, SslMode, ToSql, Row, Rows, Statement};

    use self::core::iter::Map;
    use self::pg_typeprovider::util::Joinable;

    pg_table!(users)

    // TODO: this currently will modify the database and leave it in an
    // unclean state.
    #[test]
    fn test_user_template() {
        let user = UserInsert {
            email: "roeschinc@gmail.com".to_string(),
            first_name: "Jared".to_string(),
            last_name: "Roesch".to_string(),
            access_token: "is mine".to_string(),
            created_at: now().to_timespec(),
            updated_at: now().to_timespec(),
            github_username: "jroesch".to_string(),
            password_digest: "faksdflasfjslf".to_string()
        };

        let conn = Connection::connect("postgres://jroesch@localhost/gradr-production", &SslMode::None)
            .unwrap();
        
        user.insert(&conn);
    }

    // TODO: this test should not compile.  We don't want to allow
    // users to warp-in a record.
    #[test]
    fn test_user_full() {
        let user = User {
            id: 0,
            email: "roeschinc@gmail.com".to_string(),
            first_name: "Jared".to_string(),
            last_name: "Roesch".to_string(),
            access_token: "is mine".to_string(),
            created_at: now().to_timespec(),
            updated_at: now().to_timespec(),
            github_username: "jroesch".to_string(),
            password_digest: "faksdflasfjslf".to_string()
        };

        assert_eq!("roeschinc@gmail.com", user.email.as_slice());
    }
}
