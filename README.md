pg-typeprovider
===============

A macro that implements a type provider for PostgreSQL schema. The macro takes the table name as an argument
and will deal with the capitlization and depluralization of it into a structure name. It will then read the
schema and generate a corresponding structure. 

This is very alpha quality work and is being used for a class project that consists of an application that
defines the schema in ActiveRecord, and the idea is to allow Rust to attach to the database and automatically
deal with any schema updates by reading the database.

Feel free to submit PRs, comments, ect.
