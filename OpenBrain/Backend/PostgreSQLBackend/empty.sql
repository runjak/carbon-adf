--
-- PostgreSQL database dump
--

-- Dumped from database version 9.1.5
-- Dumped by pg_dump version 9.1.5
-- Started on 2012-09-26 01:16:58 CEST

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 173 (class 3079 OID 11677)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 1968 (class 0 OID 0)
-- Dependencies: 173
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- TOC entry 169 (class 1259 OID 16497)
-- Dependencies: 1929 6
-- Name: DiscussionChoices; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "DiscussionChoices" (
    discussionid integer NOT NULL,
    informationid integer NOT NULL,
    votes integer DEFAULT 0 NOT NULL
);


ALTER TABLE public."DiscussionChoices" OWNER TO sicarius;

--
-- TOC entry 168 (class 1259 OID 16485)
-- Dependencies: 1928 6
-- Name: DiscussionInfo; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "DiscussionInfo" (
    discussionid integer NOT NULL,
    complete integer,
    deadline timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public."DiscussionInfo" OWNER TO sicarius;

--
-- TOC entry 167 (class 1259 OID 16483)
-- Dependencies: 6 168
-- Name: DiscussionInfo_discussionid_seq; Type: SEQUENCE; Schema: public; Owner: sicarius
--

CREATE SEQUENCE "DiscussionInfo_discussionid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."DiscussionInfo_discussionid_seq" OWNER TO sicarius;

--
-- TOC entry 1971 (class 0 OID 0)
-- Dependencies: 167
-- Name: DiscussionInfo_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sicarius
--

ALTER SEQUENCE "DiscussionInfo_discussionid_seq" OWNED BY "DiscussionInfo".discussionid;


--
-- TOC entry 1972 (class 0 OID 0)
-- Dependencies: 167
-- Name: DiscussionInfo_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: sicarius
--

SELECT pg_catalog.setval('"DiscussionInfo_discussionid_seq"', 1, false);


--
-- TOC entry 170 (class 1259 OID 16511)
-- Dependencies: 1930 6
-- Name: DiscussionParticipants; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "DiscussionParticipants" (
    discussionid integer NOT NULL,
    voted boolean DEFAULT false NOT NULL,
    userid integer NOT NULL
);


ALTER TABLE public."DiscussionParticipants" OWNER TO sicarius;

--
-- TOC entry 164 (class 1259 OID 16447)
-- Dependencies: 1925 6
-- Name: Information; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "Information" (
    informationid integer NOT NULL,
    author integer NOT NULL,
    creation timestamp without time zone DEFAULT now() NOT NULL,
    deletion timestamp without time zone,
    description text,
    title character varying(255) NOT NULL,
    mediaid integer NOT NULL
);


ALTER TABLE public."Information" OWNER TO sicarius;

--
-- TOC entry 163 (class 1259 OID 16445)
-- Dependencies: 164 6
-- Name: Information_informationid_seq; Type: SEQUENCE; Schema: public; Owner: sicarius
--

CREATE SEQUENCE "Information_informationid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Information_informationid_seq" OWNER TO sicarius;

--
-- TOC entry 1975 (class 0 OID 0)
-- Dependencies: 163
-- Name: Information_informationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sicarius
--

ALTER SEQUENCE "Information_informationid_seq" OWNED BY "Information".informationid;


--
-- TOC entry 1976 (class 0 OID 0)
-- Dependencies: 163
-- Name: Information_informationid_seq; Type: SEQUENCE SET; Schema: public; Owner: sicarius
--

SELECT pg_catalog.setval('"Information_informationid_seq"', 1, false);


--
-- TOC entry 166 (class 1259 OID 16469)
-- Dependencies: 6
-- Name: Media; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "Media" (
    mediaid integer NOT NULL,
    content text,
    collectiontype integer,
    discussionid integer
);


ALTER TABLE public."Media" OWNER TO sicarius;

--
-- TOC entry 165 (class 1259 OID 16467)
-- Dependencies: 6 166
-- Name: Media_mediaid_seq; Type: SEQUENCE; Schema: public; Owner: sicarius
--

CREATE SEQUENCE "Media_mediaid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Media_mediaid_seq" OWNER TO sicarius;

--
-- TOC entry 1978 (class 0 OID 0)
-- Dependencies: 165
-- Name: Media_mediaid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sicarius
--

ALTER SEQUENCE "Media_mediaid_seq" OWNED BY "Media".mediaid;


--
-- TOC entry 1979 (class 0 OID 0)
-- Dependencies: 165
-- Name: Media_mediaid_seq; Type: SEQUENCE SET; Schema: public; Owner: sicarius
--

SELECT pg_catalog.setval('"Media_mediaid_seq"', 1, false);


--
-- TOC entry 172 (class 1259 OID 16527)
-- Dependencies: 1932 1933 6
-- Name: Relations; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "Relations" (
    relationid integer NOT NULL,
    comment character varying(255) DEFAULT NULL::character varying NOT NULL,
    creation timestamp without time zone DEFAULT now() NOT NULL,
    deletion timestamp without time zone,
    type integer NOT NULL,
    source integer NOT NULL,
    target integer NOT NULL
);


ALTER TABLE public."Relations" OWNER TO sicarius;

--
-- TOC entry 171 (class 1259 OID 16525)
-- Dependencies: 6 172
-- Name: Relations_relationid_seq; Type: SEQUENCE; Schema: public; Owner: sicarius
--

CREATE SEQUENCE "Relations_relationid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Relations_relationid_seq" OWNER TO sicarius;

--
-- TOC entry 1981 (class 0 OID 0)
-- Dependencies: 171
-- Name: Relations_relationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sicarius
--

ALTER SEQUENCE "Relations_relationid_seq" OWNED BY "Relations".relationid;


--
-- TOC entry 1982 (class 0 OID 0)
-- Dependencies: 171
-- Name: Relations_relationid_seq; Type: SEQUENCE SET; Schema: public; Owner: sicarius
--

SELECT pg_catalog.setval('"Relations_relationid_seq"', 1, false);


--
-- TOC entry 162 (class 1259 OID 16425)
-- Dependencies: 1921 1922 1923 6
-- Name: UserData; Type: TABLE; Schema: public; Owner: sicarius; Tablespace: 
--

CREATE TABLE "UserData" (
    userid integer NOT NULL,
    username character varying(255) NOT NULL,
    password character varying(255) NOT NULL,
    karma integer DEFAULT 0 NOT NULL,
    creation timestamp without time zone DEFAULT now() NOT NULL,
    lastlogin timestamp without time zone,
    isadmin boolean DEFAULT false NOT NULL,
    salt character varying(255) NOT NULL,
    actionkey character varying(255),
    profile integer
);


ALTER TABLE public."UserData" OWNER TO sicarius;

--
-- TOC entry 161 (class 1259 OID 16421)
-- Dependencies: 6 162
-- Name: UserData_userid_seq; Type: SEQUENCE; Schema: public; Owner: sicarius
--

CREATE SEQUENCE "UserData_userid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."UserData_userid_seq" OWNER TO sicarius;

--
-- TOC entry 1984 (class 0 OID 0)
-- Dependencies: 161
-- Name: UserData_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: sicarius
--

ALTER SEQUENCE "UserData_userid_seq" OWNED BY "UserData".userid;


--
-- TOC entry 1985 (class 0 OID 0)
-- Dependencies: 161
-- Name: UserData_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: sicarius
--

SELECT pg_catalog.setval('"UserData_userid_seq"', 12, true);


--
-- TOC entry 1927 (class 2604 OID 16488)
-- Dependencies: 167 168 168
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "DiscussionInfo" ALTER COLUMN discussionid SET DEFAULT nextval('"DiscussionInfo_discussionid_seq"'::regclass);


--
-- TOC entry 1924 (class 2604 OID 16450)
-- Dependencies: 163 164 164
-- Name: informationid; Type: DEFAULT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Information" ALTER COLUMN informationid SET DEFAULT nextval('"Information_informationid_seq"'::regclass);


--
-- TOC entry 1926 (class 2604 OID 16472)
-- Dependencies: 165 166 166
-- Name: mediaid; Type: DEFAULT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Media" ALTER COLUMN mediaid SET DEFAULT nextval('"Media_mediaid_seq"'::regclass);


--
-- TOC entry 1931 (class 2604 OID 16530)
-- Dependencies: 171 172 172
-- Name: relationid; Type: DEFAULT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Relations" ALTER COLUMN relationid SET DEFAULT nextval('"Relations_relationid_seq"'::regclass);


--
-- TOC entry 1920 (class 2604 OID 16428)
-- Dependencies: 162 161 162
-- Name: userid; Type: DEFAULT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "UserData" ALTER COLUMN userid SET DEFAULT nextval('"UserData_userid_seq"'::regclass);


--
-- TOC entry 1958 (class 0 OID 16497)
-- Dependencies: 169 1961
-- Data for Name: DiscussionChoices; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "DiscussionChoices" (discussionid, informationid, votes) FROM stdin;
\.


--
-- TOC entry 1957 (class 0 OID 16485)
-- Dependencies: 168 1961
-- Data for Name: DiscussionInfo; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "DiscussionInfo" (discussionid, complete, deadline) FROM stdin;
\.


--
-- TOC entry 1959 (class 0 OID 16511)
-- Dependencies: 170 1961
-- Data for Name: DiscussionParticipants; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "DiscussionParticipants" (discussionid, voted, userid) FROM stdin;
\.


--
-- TOC entry 1955 (class 0 OID 16447)
-- Dependencies: 164 1961
-- Data for Name: Information; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "Information" (informationid, author, creation, deletion, description, title, mediaid) FROM stdin;
\.


--
-- TOC entry 1956 (class 0 OID 16469)
-- Dependencies: 166 1961
-- Data for Name: Media; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "Media" (mediaid, content, collectiontype, discussionid) FROM stdin;
\.


--
-- TOC entry 1960 (class 0 OID 16527)
-- Dependencies: 172 1961
-- Data for Name: Relations; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "Relations" (relationid, comment, creation, deletion, type, source, target) FROM stdin;
\.


--
-- TOC entry 1954 (class 0 OID 16425)
-- Dependencies: 162 1961
-- Data for Name: UserData; Type: TABLE DATA; Schema: public; Owner: sicarius
--

COPY "UserData" (userid, username, password, karma, creation, lastlogin, isadmin, salt, actionkey, profile) FROM stdin;
\.


--
-- TOC entry 1941 (class 2606 OID 16491)
-- Dependencies: 168 168 1962
-- Name: DiscussionInfo.discussionid; Type: CONSTRAINT; Schema: public; Owner: sicarius; Tablespace: 
--

ALTER TABLE ONLY "DiscussionInfo"
    ADD CONSTRAINT "DiscussionInfo.discussionid" PRIMARY KEY (discussionid);


--
-- TOC entry 1937 (class 2606 OID 16456)
-- Dependencies: 164 164 1962
-- Name: Information.informationid; Type: CONSTRAINT; Schema: public; Owner: sicarius; Tablespace: 
--

ALTER TABLE ONLY "Information"
    ADD CONSTRAINT "Information.informationid" PRIMARY KEY (informationid);


--
-- TOC entry 1939 (class 2606 OID 16477)
-- Dependencies: 166 166 1962
-- Name: Media.mediaid; Type: CONSTRAINT; Schema: public; Owner: sicarius; Tablespace: 
--

ALTER TABLE ONLY "Media"
    ADD CONSTRAINT "Media.mediaid" PRIMARY KEY (mediaid);


--
-- TOC entry 1943 (class 2606 OID 16534)
-- Dependencies: 172 172 1962
-- Name: Relations.relationid; Type: CONSTRAINT; Schema: public; Owner: sicarius; Tablespace: 
--

ALTER TABLE ONLY "Relations"
    ADD CONSTRAINT "Relations.relationid" PRIMARY KEY (relationid);


--
-- TOC entry 1935 (class 2606 OID 16437)
-- Dependencies: 162 162 1962
-- Name: UserData.userid; Type: CONSTRAINT; Schema: public; Owner: sicarius; Tablespace: 
--

ALTER TABLE ONLY "UserData"
    ADD CONSTRAINT "UserData.userid" PRIMARY KEY (userid);


--
-- TOC entry 1948 (class 2606 OID 16501)
-- Dependencies: 168 1940 169 1962
-- Name: DiscussionChoices.discussionid; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "DiscussionChoices"
    ADD CONSTRAINT "DiscussionChoices.discussionid" FOREIGN KEY (discussionid) REFERENCES "DiscussionInfo"(discussionid);


--
-- TOC entry 1949 (class 2606 OID 16506)
-- Dependencies: 164 169 1936 1962
-- Name: DiscussionChoices.informationid; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "DiscussionChoices"
    ADD CONSTRAINT "DiscussionChoices.informationid" FOREIGN KEY (informationid) REFERENCES "Information"(informationid);


--
-- TOC entry 1950 (class 2606 OID 16515)
-- Dependencies: 168 1940 170 1962
-- Name: DiscussionParticipants.discussionid; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "DiscussionParticipants"
    ADD CONSTRAINT "DiscussionParticipants.discussionid" FOREIGN KEY (discussionid) REFERENCES "DiscussionInfo"(discussionid);


--
-- TOC entry 1951 (class 2606 OID 16520)
-- Dependencies: 162 170 1934 1962
-- Name: DiscussionParticipants.userid; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "DiscussionParticipants"
    ADD CONSTRAINT "DiscussionParticipants.userid" FOREIGN KEY (userid) REFERENCES "UserData"(userid);


--
-- TOC entry 1945 (class 2606 OID 16457)
-- Dependencies: 164 162 1934 1962
-- Name: Information.author; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Information"
    ADD CONSTRAINT "Information.author" FOREIGN KEY (author) REFERENCES "UserData"(userid);


--
-- TOC entry 1946 (class 2606 OID 16478)
-- Dependencies: 164 166 1938 1962
-- Name: Information.mediaid; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Information"
    ADD CONSTRAINT "Information.mediaid" FOREIGN KEY (mediaid) REFERENCES "Media"(mediaid);


--
-- TOC entry 1947 (class 2606 OID 16492)
-- Dependencies: 168 166 1940 1962
-- Name: Media.discussionid; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Media"
    ADD CONSTRAINT "Media.discussionid" FOREIGN KEY (discussionid) REFERENCES "DiscussionInfo"(discussionid);


--
-- TOC entry 1952 (class 2606 OID 16535)
-- Dependencies: 164 1936 172 1962
-- Name: Relations.source; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Relations"
    ADD CONSTRAINT "Relations.source" FOREIGN KEY (source) REFERENCES "Information"(informationid);


--
-- TOC entry 1953 (class 2606 OID 16540)
-- Dependencies: 172 164 1936 1962
-- Name: Relations.target; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "Relations"
    ADD CONSTRAINT "Relations.target" FOREIGN KEY (target) REFERENCES "Information"(informationid);


--
-- TOC entry 1944 (class 2606 OID 16462)
-- Dependencies: 1936 162 164 1962
-- Name: UserData.profile; Type: FK CONSTRAINT; Schema: public; Owner: sicarius
--

ALTER TABLE ONLY "UserData"
    ADD CONSTRAINT "UserData.profile" FOREIGN KEY (profile) REFERENCES "Information"(informationid);


--
-- TOC entry 1967 (class 0 OID 0)
-- Dependencies: 6
-- Name: public; Type: ACL; Schema: -; Owner: sicarius
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM sicarius;
GRANT ALL ON SCHEMA public TO sicarius;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- TOC entry 1969 (class 0 OID 0)
-- Dependencies: 169
-- Name: DiscussionChoices; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "DiscussionChoices" FROM PUBLIC;
REVOKE ALL ON TABLE "DiscussionChoices" FROM sicarius;
GRANT ALL ON TABLE "DiscussionChoices" TO sicarius;
GRANT ALL ON TABLE "DiscussionChoices" TO PUBLIC;


--
-- TOC entry 1970 (class 0 OID 0)
-- Dependencies: 168
-- Name: DiscussionInfo; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "DiscussionInfo" FROM PUBLIC;
REVOKE ALL ON TABLE "DiscussionInfo" FROM sicarius;
GRANT ALL ON TABLE "DiscussionInfo" TO sicarius;
GRANT ALL ON TABLE "DiscussionInfo" TO PUBLIC;


--
-- TOC entry 1973 (class 0 OID 0)
-- Dependencies: 170
-- Name: DiscussionParticipants; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "DiscussionParticipants" FROM PUBLIC;
REVOKE ALL ON TABLE "DiscussionParticipants" FROM sicarius;
GRANT ALL ON TABLE "DiscussionParticipants" TO sicarius;
GRANT ALL ON TABLE "DiscussionParticipants" TO PUBLIC;


--
-- TOC entry 1974 (class 0 OID 0)
-- Dependencies: 164
-- Name: Information; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "Information" FROM PUBLIC;
REVOKE ALL ON TABLE "Information" FROM sicarius;
GRANT ALL ON TABLE "Information" TO sicarius;
GRANT ALL ON TABLE "Information" TO PUBLIC;


--
-- TOC entry 1977 (class 0 OID 0)
-- Dependencies: 166
-- Name: Media; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "Media" FROM PUBLIC;
REVOKE ALL ON TABLE "Media" FROM sicarius;
GRANT ALL ON TABLE "Media" TO sicarius;
GRANT ALL ON TABLE "Media" TO PUBLIC;


--
-- TOC entry 1980 (class 0 OID 0)
-- Dependencies: 172
-- Name: Relations; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "Relations" FROM PUBLIC;
REVOKE ALL ON TABLE "Relations" FROM sicarius;
GRANT ALL ON TABLE "Relations" TO sicarius;
GRANT ALL ON TABLE "Relations" TO PUBLIC;


--
-- TOC entry 1983 (class 0 OID 0)
-- Dependencies: 162
-- Name: UserData; Type: ACL; Schema: public; Owner: sicarius
--

REVOKE ALL ON TABLE "UserData" FROM PUBLIC;
REVOKE ALL ON TABLE "UserData" FROM sicarius;
GRANT ALL ON TABLE "UserData" TO sicarius;
GRANT ALL ON TABLE "UserData" TO PUBLIC;


--
-- TOC entry 1451 (class 826 OID 16545)
-- Dependencies: 6 1962
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM PUBLIC;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON TABLES  TO PUBLIC;


-- Completed on 2012-09-26 01:16:58 CEST

--
-- PostgreSQL database dump complete
--

