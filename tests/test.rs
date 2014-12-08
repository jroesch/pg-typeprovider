#![feature(phase)]
#[phase(plugin)]
extern crate pg_typeprovider;

// TODO: this is very weird; I need to put this in a module, and pg_typeprovider
// must be put in twice.  This might be a bug in Rust; ask Jared.

// TODO: teardown database
mod testing {
    extern crate core;
    extern crate time;
    extern crate postgres;
    extern crate pg_typeprovider;

    use self::time::{now, Timespec};

    use self::postgres::{Connection, SslMode, ToSql};

    use self::pg_typeprovider::util::Joinable;

    pg_table!(users)

    fn make_conn() -> Connection {
        Connection::connect(
            "postgres://jroesch@localhost/gradr-test", &SslMode::None).unwrap()
    }

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

        let conn = make_conn();
        user.insert(&conn);
    }

    #[test]
    fn test_search_template() {
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

        let conn = make_conn();
        user.insert(&conn);
        let matching = UserSearch {
            id: None,
            first_name: Some("Jared".to_string()),
            email: None,
            last_name: None,
            access_token: None,
            created_at: None,
            updated_at: None,
            github_username: None,
            password_digest: None
        }.search(&conn, Some(1));
        assert!(matching.len() > 0);
        assert_eq!(matching[0].last_name.as_slice(), "Roesch");
    }

    #[test]
    fn test_update_template() {
        let user = UserInsert {
            email: "kyledewey@cs.ucsb.edu".to_string(),
            first_name: "Kyle".to_string(),
            last_name: "Dewey".to_string(),
            access_token: "is mine".to_string(),
            created_at: now().to_timespec(),
            updated_at: now().to_timespec(),
            github_username: "kyledewey".to_string(),
            password_digest: "faksdflasfjslf".to_string()
        };

        let conn = make_conn();
        user.insert(&conn);

        let matching = UserSearch {
            id: None,
            first_name: Some("Kyle".to_string()),
            email: None,
            last_name: None,
            access_token: None,
            created_at: None,
            updated_at: None,
            github_username: None,
            password_digest: None
        }.search(&conn, Some(1));
        assert!(matching.len() > 0);

        let in_db = &matching[0];

        assert_eq!(in_db.last_name.as_slice(), "Dewey");

        let id = in_db.get_id();
        let update =
            UserUpdate {
                where_id: Some(id), 
                where_first_name: None,
                first_name_to: Some("billy".to_string()),
                where_last_name: None,
                last_name_to: Some("bob".to_string()),
                where_email: None,
                email_to: None,
                where_access_token: None,
                access_token_to: None,
                where_created_at: None,
                created_at_to: None,
                where_updated_at: None,
                updated_at_to: None,
                where_github_username: None,
                github_username_to: None,
                where_password_digest: None,
                password_digest_to: None
            };
        let update_res = update.update(&conn);
        assert_eq!(update_res, 1);

        let matching2 = UserSearch {
            id: Some(id),
            first_name: None,
            email: None,
            last_name: None,
            access_token: None,
            created_at: None,
            updated_at: None,
            github_username: None,
            password_digest: None
        }.search(&conn, Some(1));
        assert!(matching2.len() > 0);

        let in_db2 = &matching2[0];

        assert_eq!(in_db2.get_id(), id);
        assert_eq!(in_db2.first_name.as_slice(), "billy");
        assert_eq!(in_db2.last_name.as_slice(), "bob");
    }

    // TODO: this test should not compile.  We don't want to allow
    // users to warp-in a record.  This can be fixed if we enclose
    // everything in a module.
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
