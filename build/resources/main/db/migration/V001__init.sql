CREATE SEQUENCE public."person_seq" INCREMENT 1 START 1;

CREATE TABLE public."person"
(
   "ID" bigint NOT NULL,
   "NAME" character varying(250) NOT NULL,
   "RSS" character varying(250) NOT NULL,
   "TWITTER" character varying(100),
   CONSTRAINT "PERSON_PRIMARY_KEY" PRIMARY KEY ("ID"),
   CONSTRAINT "PERSON_RSS" UNIQUE ("RSS")
);