--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: DiscussionChoices; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "DiscussionChoices" (
    discussionid integer NOT NULL,
    informationid integer NOT NULL,
    votes integer DEFAULT 0 NOT NULL
);


ALTER TABLE public."DiscussionChoices" OWNER TO mushu;

--
-- Name: DiscussionInfo; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "DiscussionInfo" (
    discussionid integer NOT NULL,
    complete integer,
    deadline timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public."DiscussionInfo" OWNER TO mushu;

--
-- Name: DiscussionInfo_discussionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE "DiscussionInfo_discussionid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."DiscussionInfo_discussionid_seq" OWNER TO mushu;

--
-- Name: DiscussionInfo_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE "DiscussionInfo_discussionid_seq" OWNED BY "DiscussionInfo".discussionid;


--
-- Name: DiscussionInfo_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('"DiscussionInfo_discussionid_seq"', 1, false);


--
-- Name: DiscussionParticipants; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "DiscussionParticipants" (
    discussionid integer NOT NULL,
    voted boolean DEFAULT false NOT NULL,
    userid integer NOT NULL
);


ALTER TABLE public."DiscussionParticipants" OWNER TO mushu;

--
-- Name: Information; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "Information" (
    informationid integer NOT NULL,
    author integer NOT NULL,
    creation timestamp with time zone DEFAULT now() NOT NULL,
    deletion timestamp with time zone,
    description text,
    title character varying(255) NOT NULL,
    mediaid integer NOT NULL
);


ALTER TABLE public."Information" OWNER TO mushu;

--
-- Name: Information_informationid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE "Information_informationid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Information_informationid_seq" OWNER TO mushu;

--
-- Name: Information_informationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE "Information_informationid_seq" OWNED BY "Information".informationid;


--
-- Name: Information_informationid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('"Information_informationid_seq"', 26, true);


--
-- Name: Media; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "Media" (
    mediaid integer NOT NULL,
    content text,
    collectiontype integer,
    discussionid integer
);


ALTER TABLE public."Media" OWNER TO mushu;

--
-- Name: Media_mediaid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE "Media_mediaid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Media_mediaid_seq" OWNER TO mushu;

--
-- Name: Media_mediaid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE "Media_mediaid_seq" OWNED BY "Media".mediaid;


--
-- Name: Media_mediaid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('"Media_mediaid_seq"', 15, true);


--
-- Name: Relations; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "Relations" (
    relationid integer NOT NULL,
    comment character varying(255) DEFAULT NULL::character varying NOT NULL,
    creation timestamp with time zone DEFAULT now() NOT NULL,
    deletion timestamp with time zone,
    type integer NOT NULL,
    source integer NOT NULL,
    target integer NOT NULL
);


ALTER TABLE public."Relations" OWNER TO mushu;

--
-- Name: Relations_relationid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE "Relations_relationid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."Relations_relationid_seq" OWNER TO mushu;

--
-- Name: Relations_relationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE "Relations_relationid_seq" OWNED BY "Relations".relationid;


--
-- Name: Relations_relationid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('"Relations_relationid_seq"', 82, true);


--
-- Name: UserData; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE "UserData" (
    userid integer NOT NULL,
    username character varying(255) NOT NULL,
    password character varying(255) NOT NULL,
    karma integer DEFAULT 0 NOT NULL,
    creation timestamp with time zone DEFAULT now() NOT NULL,
    lastlogin timestamp with time zone,
    isadmin boolean DEFAULT false NOT NULL,
    salt character varying(255) NOT NULL,
    actionkey character varying(255),
    profile integer
);


ALTER TABLE public."UserData" OWNER TO mushu;

--
-- Name: UserData_userid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE "UserData_userid_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."UserData_userid_seq" OWNER TO mushu;

--
-- Name: UserData_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE "UserData_userid_seq" OWNED BY "UserData".userid;


--
-- Name: UserData_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('"UserData_userid_seq"', 5, true);


--
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "DiscussionInfo" ALTER COLUMN discussionid SET DEFAULT nextval('"DiscussionInfo_discussionid_seq"'::regclass);


--
-- Name: informationid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Information" ALTER COLUMN informationid SET DEFAULT nextval('"Information_informationid_seq"'::regclass);


--
-- Name: mediaid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Media" ALTER COLUMN mediaid SET DEFAULT nextval('"Media_mediaid_seq"'::regclass);


--
-- Name: relationid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Relations" ALTER COLUMN relationid SET DEFAULT nextval('"Relations_relationid_seq"'::regclass);


--
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "UserData" ALTER COLUMN userid SET DEFAULT nextval('"UserData_userid_seq"'::regclass);


--
-- Data for Name: DiscussionChoices; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "DiscussionChoices" (discussionid, informationid, votes) FROM stdin;
\.


--
-- Data for Name: DiscussionInfo; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "DiscussionInfo" (discussionid, complete, deadline) FROM stdin;
\.


--
-- Data for Name: DiscussionParticipants; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "DiscussionParticipants" (discussionid, voted, userid) FROM stdin;
\.


--
-- Data for Name: Information; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "Information" (informationid, author, creation, deletion, description, title, mediaid) FROM stdin;
25	5	2012-09-29 22:53:52.201516+02	\N	Derivate Jake.	A different picture of jake.	14
21	5	2012-09-29 20:37:59.836474+02	2012-09-29 22:53:55.259998+02	A picture of jake, the dog.	Jake#1	10
26	5	2012-10-01 00:49:59.259452+02	\N	Jake keeps changing.	Jake#3	15
22	5	2012-09-29 20:38:04.725603+02	2012-10-01 00:50:02.63739+02	A different picture of jake, the dog.	Jake#2	11
\.


--
-- Data for Name: Media; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "Media" (mediaid, content, collectiontype, discussionid) FROM stdin;
10	![Yeah, it's Jake.](http://2.bp.blogspot.com/_X0JKSfcN0kw/TMS4HCNz49I/AAAAAAAAABM/-u0r6DW_q94/s1600/Adventure+Time+with+Finn+and+Jake+-+105+-+My+Two+Favorite+People+-+Memories+of+Boom+Boom+Mountain+%5B449%5D.avi_snapshot_04.55_%5B2010.05.04_22.59.31%5D.jpg)	\N	\N
11	![Oh - another picture!](http://fc00.deviantart.net/fs71/f/2010/234/4/3/Jake_the_Dog_WOAH_by_Yukaze92.png)	\N	\N
12	narf	\N	\N
13	narf	\N	\N
14	![Yeah, it's Jake.](http://www.drawinghowtodraw.com/stepbystepdrawinglessons/wp-content/uploads/2010/11/how-to-draw-jake-the-dog-finished-color.png)	\N	\N
15	![Forever changing](http://img.poptower.com/pic-34325/jake-the-dog-adventure-time.jpg "Flyyying!")	\N	\N
\.


--
-- Data for Name: Relations; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "Relations" (relationid, comment, creation, deletion, type, source, target) FROM stdin;
73		2012-09-29 20:38:04.725603+02	\N	0	21	22
76		2012-09-29 22:53:52.201516+02	\N	0	21	25
77		2012-10-01 00:49:59.259452+02	\N	0	21	26
78		2012-10-01 00:49:59.259452+02	\N	0	22	26
79	Flying on a chair is kind of dangerous, therefore attacking.	2012-10-17 16:23:38.786344+02	2012-10-29 22:11:12.998153+01	1	26	25
81	Happy Jake defends the white background.	2012-10-29 22:13:55.235639+01	\N	2	21	22
82	Delete this nonsense!	2012-10-29 22:15:24.682321+01	\N	1	22	26
80	Flying chairs don't like white backgrounds.	2012-10-29 22:11:19.37604+01	2012-10-29 22:21:01.468592+01	1	26	22
\.


--
-- Data for Name: UserData; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY "UserData" (userid, username, password, karma, creation, lastlogin, isadmin, salt, actionkey, profile) FROM stdin;
5	test	6d006996ae2a39d9e3266194f68a538a34395b3f8033a3ce490de066e0471942be6a8ce5b36e8cb0015a79e8ff3c0073dacdaa8330256915f3bf4d8482cc3c45	0	2012-09-28 18:55:16.714331+02	2012-11-12 15:44:15.47559+01	f	{b{vopspz{ubtzht|	mbnvpyokykanafc	25
\.


--
-- Name: DiscussionInfo.discussionid; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY "DiscussionInfo"
    ADD CONSTRAINT "DiscussionInfo.discussionid" PRIMARY KEY (discussionid);


--
-- Name: Information.informationid; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY "Information"
    ADD CONSTRAINT "Information.informationid" PRIMARY KEY (informationid);


--
-- Name: Media.mediaid; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY "Media"
    ADD CONSTRAINT "Media.mediaid" PRIMARY KEY (mediaid);


--
-- Name: Relations.relationid; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY "Relations"
    ADD CONSTRAINT "Relations.relationid" PRIMARY KEY (relationid);


--
-- Name: UserData.userid; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY "UserData"
    ADD CONSTRAINT "UserData.userid" PRIMARY KEY (userid);


--
-- Name: DiscussionChoices.discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "DiscussionChoices"
    ADD CONSTRAINT "DiscussionChoices.discussionid" FOREIGN KEY (discussionid) REFERENCES "DiscussionInfo"(discussionid);


--
-- Name: DiscussionChoices.informationid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "DiscussionChoices"
    ADD CONSTRAINT "DiscussionChoices.informationid" FOREIGN KEY (informationid) REFERENCES "Information"(informationid);


--
-- Name: DiscussionParticipants.discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "DiscussionParticipants"
    ADD CONSTRAINT "DiscussionParticipants.discussionid" FOREIGN KEY (discussionid) REFERENCES "DiscussionInfo"(discussionid);


--
-- Name: DiscussionParticipants.userid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "DiscussionParticipants"
    ADD CONSTRAINT "DiscussionParticipants.userid" FOREIGN KEY (userid) REFERENCES "UserData"(userid);


--
-- Name: Information.author; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Information"
    ADD CONSTRAINT "Information.author" FOREIGN KEY (author) REFERENCES "UserData"(userid);


--
-- Name: Information.mediaid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Information"
    ADD CONSTRAINT "Information.mediaid" FOREIGN KEY (mediaid) REFERENCES "Media"(mediaid);


--
-- Name: Media.discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Media"
    ADD CONSTRAINT "Media.discussionid" FOREIGN KEY (discussionid) REFERENCES "DiscussionInfo"(discussionid);


--
-- Name: Relations.source; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Relations"
    ADD CONSTRAINT "Relations.source" FOREIGN KEY (source) REFERENCES "Information"(informationid);


--
-- Name: Relations.target; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "Relations"
    ADD CONSTRAINT "Relations.target" FOREIGN KEY (target) REFERENCES "Information"(informationid);


--
-- Name: UserData.profile; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY "UserData"
    ADD CONSTRAINT "UserData.profile" FOREIGN KEY (profile) REFERENCES "Information"(informationid);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO mushu;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: DiscussionChoices; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "DiscussionChoices" FROM PUBLIC;
REVOKE ALL ON TABLE "DiscussionChoices" FROM mushu;
GRANT ALL ON TABLE "DiscussionChoices" TO mushu;
GRANT ALL ON TABLE "DiscussionChoices" TO PUBLIC;


--
-- Name: DiscussionInfo; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "DiscussionInfo" FROM PUBLIC;
REVOKE ALL ON TABLE "DiscussionInfo" FROM mushu;
GRANT ALL ON TABLE "DiscussionInfo" TO mushu;
GRANT ALL ON TABLE "DiscussionInfo" TO PUBLIC;


--
-- Name: DiscussionParticipants; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "DiscussionParticipants" FROM PUBLIC;
REVOKE ALL ON TABLE "DiscussionParticipants" FROM mushu;
GRANT ALL ON TABLE "DiscussionParticipants" TO mushu;
GRANT ALL ON TABLE "DiscussionParticipants" TO PUBLIC;


--
-- Name: Information; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "Information" FROM PUBLIC;
REVOKE ALL ON TABLE "Information" FROM mushu;
GRANT ALL ON TABLE "Information" TO mushu;
GRANT ALL ON TABLE "Information" TO PUBLIC;


--
-- Name: Media; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "Media" FROM PUBLIC;
REVOKE ALL ON TABLE "Media" FROM mushu;
GRANT ALL ON TABLE "Media" TO mushu;
GRANT ALL ON TABLE "Media" TO PUBLIC;


--
-- Name: Relations; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "Relations" FROM PUBLIC;
REVOKE ALL ON TABLE "Relations" FROM mushu;
GRANT ALL ON TABLE "Relations" TO mushu;
GRANT ALL ON TABLE "Relations" TO PUBLIC;


--
-- Name: UserData; Type: ACL; Schema: public; Owner: mushu
--

REVOKE ALL ON TABLE "UserData" FROM PUBLIC;
REVOKE ALL ON TABLE "UserData" FROM mushu;
GRANT ALL ON TABLE "UserData" TO mushu;
GRANT ALL ON TABLE "UserData" TO PUBLIC;


--
-- Name: DEFAULT PRIVILEGES FOR TABLES; Type: DEFAULT ACL; Schema: public; Owner: postgres
--

ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM PUBLIC;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public REVOKE ALL ON TABLES  FROM postgres;
ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT ALL ON TABLES  TO PUBLIC;


--
-- PostgreSQL database dump complete
--

