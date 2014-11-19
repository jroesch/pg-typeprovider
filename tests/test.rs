#![feature(phase)]
#[phase(plugin)]
extern crate pg_typeprovider;
extern crate time;

use time::{now, Timespec};

pg_table!(users)

#[test]
fn test_user_template() {
    let user = UserTemplate {
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