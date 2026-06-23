-- Create a table for users
CREATE TABLE IF NOT EXISTS users (
  id UUID DEFAULT gen_random_uuid(),
  email TEXT NOT NULL,
  password TEXT NOT NULL,
  first_name TEXT,
  last_name TEXT,
  company TEXT,
  role TEXT NOT NULL
);
-- Add a primary key
ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (id);
-- Insert a test users
INSERT INTO users (
    id,
    email,
    password,
    first_name,
    last_name,
    company,
    role
  )
VALUES (
    '843df718-ec6e-4d49-9289-f799c0f40073',
    'john_test@email.com',
    '$2a$10$f7SiL6BEPbooqYkWxTvNtebX80UqAZmvFPXwJ5t452Q.kHkU.nidO',
    'John',
    'Wick',
    'Continental',
    'ADMIN'
  );
INSERT INTO users (
    id,
    email,
    password,
    first_name,
    last_name,
    company,
    role
  )
VALUES (
    '843df718-ec6e-4d49-9289-f799c0f40074',
    'anna_test@email.com',
    'hashedpassword',
    'Anna',
    'Belle',
    'Continental',
    'RECRUITER'
  );