create extension if not exists "uuid-ossp";

-- Accessing the AI Server:
create table if not exists account (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , name varchar(255) not null
);

create table if not exists authentication (
  uid serial primary key
  , account_fk int not null references account(uid)
  , method int not null default 1     -- 1: md5.
  , secret varchar(255) not null
  , status int not null default 1     -- 1: active, 2: disabled, 3: deleted
  , created_at timestamp not null default now()
);

create table if not exists cconnection (
  uid serial primary key
  , account_fk int not null references account(uid)
  , created_at timestamp not null default now()
  , token text not null
);


create table if not exists ccancel (
  connection_fk int not null references cconnection(uid)
  , created_at timestamp not null default now()
  , reason text not null
);


-- Engaging operations on the services:
create table if not exists crequest (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , account_fk int not null references account(uid)
  , function_eid uuid not null
  , params jsonb not null
  , created_at timestamp not null default now()
);

-- Stages of execution of a request:
create table if not exists ReqExec (
  uid serial primary key
  , crequest_fk int not null references crequest(uid)
  , created_at timestamp not null default now()
  , kind int not null default 1     -- 1: submission, 2: reply, 3: replay, 4: timeout, 5: error.
  , notes text
);

-- Result of a request (when correctly executed):
create table if not exists cresponse (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , crequest_fk int not null references crequest(uid)
  , created_at timestamp not null default now()
  , kind int not null default 1     -- 1: plain text, 2: json, 3: base64, 4: markdown, 5: reference to asset.
  , content text
);


-- Context grouping multiple requests/responses into one body of work:
create table if not exists ccontext (
  uid serial primary key
);


-- User's session with the AI Server:
create table if not exists csession (
  uid serial primary key
  , account_fk int not null references account(uid)
  , created_at timestamp not null default now()
  , ended_at timestamp
  , context_fk int not null references ccontext(uid)
);


-- Feedback from the user:
create table if not exists cfeedback (
  uid serial primary key
  , response_fk int not null references cresponse(uid)
  , content text not null
  , created_at timestamp not null default now()
  );


create table if not exists Memory (
  uid serial primary key
  , context_fk int not null references ccontext(uid)
  , created_at timestamp not null default now()
  , version int not null default 1
  , grouping_fk int not null references Memory(uid)
  , response_fk int references cresponse(uid)
  , content text
);

-- Managing facts and information into a Knowledge Base:
create table if not exists Knowledge (
  uid serial primary key
  , created_at timestamp not null default now()
  , version int not null default 1
  , kind int not null default 1     -- 1: plain text, 2: json, 3: html, 4: markdown, 5: reference to asset.
  , content text
);

create table if not exists KnowConn (
  uid serial primary key
  , fact_fk int not null references Knowledge(uid)
  , assoc_fk int references OntoNode(uid)
  , created_at timestamp not null default now()
);

create table if not exists ontology (
  uid serial primary key
  , label text not null
);

create table if not exists OntoNode (
  uid serial primary key
  , ontology_fk int not null references ontology(uid)
  , label text not null
);

-- Organization management:
create table if not exists OrgEntity (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , name text not null
);

create table if not exists OrgEntityConn (
  uid serial primary key
  , org_fk int not null references OrgEntity(uid)
  , account_fk int not null references account(uid)
);

-- Financial operations:
create table if not exists Transaction (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , account_fk int not null references account(uid)
  , created_at timestamp not null default now()
  , amount decimal(8,2) not null
  , currency varchar(3) not null
  , status int not null default 1     -- 1: pending, 2: completed, 3: aborted.
  , notes text not null
);


create table if not exists Invoice (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , account_fk int not null references account(uid)
  , created_at timestamp not null default now()
  , amount decimal(8,2) not null
  , currency varchar(3) not null
  , notes text not null
);

create table if not exists InvoiceItem (
  uid serial primary key
  , invoice_fk int not null references Invoice(uid)
  , transaction_fk int not null references Transaction(uid)
);

create table if not exists Payment (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , account_fk int not null references account(uid)
  , invoice_fk int references Invoice(uid)
  , created_at timestamp not null default now()
  , amount decimal(8,2) not null
  , currency varchar(3) not null
  , notes text not null
);

create table if not exists FinancialAcct (
  uid serial primary key
  , balance decimal(8,2) not null
  , currency varchar(3) not null
  , updated_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);


-- Services that the AI Server proxies for:
create table if not exists aiservice (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , name text not null
  , description text not null
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);


create table if not exists servfunction (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , aiservice_fk int not null references aiservice(uid)
  , label text not null
  , pricingRule text not null
  , privilegeRule text not null
  , endpoint text not null
  , category varchar(32)[] not null
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);

-- Account information supplied by the service provider:
create table if not exists servaccount (
  uid serial primary key
  , service_fk int not null references aiservice(uid)
  , acctName varchar(255) not null
  , authMethod int not null default 1     -- 1: apiKey, 2: oauth2.
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
);

-- Authentication supplied by the service provider:
create table if not exists servAuth (
  servaccount_fk int not null references servaccount(uid)
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
  , idlabel text not null
  , secret text not null
  , created_at timestamp not null default now()
);


create table if not exists servaccess (
  servfct_fk int not null references servfunction(uid)
  , caccount int not null references account(uid)
);

-- Asset management:
create table if not exists Asset (
  uid serial primary key
  , eid uuid not null default uuid_generate_v4()
  , created_at timestamp not null default now()
  , status int not null default 1     -- 1: active, 2: disabled, 3: retired.
  , name text
  , description text
  , contentType varchar(255)
  , size bigint not null
  , version int not null default 1
  , notes text
);

