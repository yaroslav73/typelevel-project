CREATE DATABASE board;
\ c board;
CREATE TABLE jobs(
  id UUID DEFAULT gen_random_uuid(),
  timestamp TIMESTAMP NOT NUll,
  owner_email TEXT NOT NULL,
  company TEXT NOT NULL,
  title TEXT NOT NULL,
  description TEXT NOT NULL,
  external_url TEXT NOT NULL,
  location TEXT,
  remote BOOLEAN NOT NULL DEFAULT false,
  salary INTEGER,
  currency TEXT,
  country TEXT,
  tags TEXT [],
  image TEXT,
  seniority TEXT,
  other TEXT,
  active BOOLEAN NOT NULL DEFAULT false
);
ALTER TABLE jobs
ADD CONSTRAINT pk_jobs PRIMARY KEY (id);
CREATE TABLE users(
  email TEXT NOT NULL,
  password TEXT NOT NULL,
  firstName TEXT NOT NULL,
  lastName TEXT,
  company TEXT,
  role TEXT NOT NULL,
);
ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (email);