--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: openbrain; Type: COMMENT; Schema: -; Owner: mushu
--

COMMENT ON DATABASE openbrain IS 'The openbrain development database. It uses the schema from the design document with the date of 2013-04-29.';


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

SET default_with_oids = true;

--
-- Name: articles; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE articles (
    articleid integer NOT NULL,
    descriptionid integer NOT NULL,
    content text NOT NULL
);


ALTER TABLE public.articles OWNER TO mushu;

--
-- Name: articles_articleid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE articles_articleid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.articles_articleid_seq OWNER TO mushu;

--
-- Name: articles_articleid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE articles_articleid_seq OWNED BY articles.articleid;


--
-- Name: articles_descriptionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE articles_descriptionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.articles_descriptionid_seq OWNER TO mushu;

--
-- Name: articles_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE articles_descriptionid_seq OWNED BY articles.descriptionid;


--
-- Name: children; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE children (
    parent integer NOT NULL,
    child integer NOT NULL
);


ALTER TABLE public.children OWNER TO mushu;

--
-- Name: children_child_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE children_child_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.children_child_seq OWNER TO mushu;

--
-- Name: children_child_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE children_child_seq OWNED BY children.child;


--
-- Name: children_parent_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE children_parent_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.children_parent_seq OWNER TO mushu;

--
-- Name: children_parent_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE children_parent_seq OWNED BY children.parent;


--
-- Name: choices; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE choices (
    resultid integer NOT NULL,
    collectionid integer NOT NULL,
    votes integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.choices OWNER TO mushu;

--
-- Name: choices_collectionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE choices_collectionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.choices_collectionid_seq OWNER TO mushu;

--
-- Name: choices_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE choices_collectionid_seq OWNED BY choices.collectionid;


--
-- Name: choices_resultid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE choices_resultid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.choices_resultid_seq OWNER TO mushu;

--
-- Name: choices_resultid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE choices_resultid_seq OWNED BY choices.resultid;


--
-- Name: collectedarticles; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE collectedarticles (
    collectionid integer NOT NULL,
    articleid integer NOT NULL,
    pos_x smallint DEFAULT 500 NOT NULL,
    pos_y smallint DEFAULT 500 NOT NULL,
    accepted boolean,
    condition text DEFAULT ''::text NOT NULL,
    customcondition boolean DEFAULT false NOT NULL
);


ALTER TABLE public.collectedarticles OWNER TO mushu;

--
-- Name: COLUMN collectedarticles.pos_x; Type: COMMENT; Schema: public; Owner: mushu
--

COMMENT ON COLUMN collectedarticles.pos_x IS 'x position of an article when displayed in a graph.';


--
-- Name: COLUMN collectedarticles.pos_y; Type: COMMENT; Schema: public; Owner: mushu
--

COMMENT ON COLUMN collectedarticles.pos_y IS 'y position of an article when displayed in a graph';


--
-- Name: COLUMN collectedarticles.accepted; Type: COMMENT; Schema: public; Owner: mushu
--

COMMENT ON COLUMN collectedarticles.accepted IS 'Null means unknown';


--
-- Name: COLUMN collectedarticles.condition; Type: COMMENT; Schema: public; Owner: mushu
--

COMMENT ON COLUMN collectedarticles.condition IS 'Acceptance condition for an article. Can be set by a client or be generated automatically. Because openBrain needs to parse/generate this anyway, it is stored as text.';


--
-- Name: COLUMN collectedarticles.customcondition; Type: COMMENT; Schema: public; Owner: mushu
--

COMMENT ON COLUMN collectedarticles.customcondition IS 'true if the condition was set by a client. This also means that it should not be overwritten by openBrain except a client requests so.';


--
-- Name: collectedarticles_articleid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE collectedarticles_articleid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.collectedarticles_articleid_seq OWNER TO mushu;

--
-- Name: collectedarticles_articleid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collectedarticles_articleid_seq OWNED BY collectedarticles.articleid;


--
-- Name: collectedarticles_collectionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE collectedarticles_collectionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.collectedarticles_collectionid_seq OWNER TO mushu;

--
-- Name: collectedarticles_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collectedarticles_collectionid_seq OWNED BY collectedarticles.collectionid;


--
-- Name: collections; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE collections (
    collectionid integer NOT NULL,
    descriptionid integer NOT NULL
);


ALTER TABLE public.collections OWNER TO mushu;

--
-- Name: collections_collectionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE collections_collectionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.collections_collectionid_seq OWNER TO mushu;

--
-- Name: collections_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collections_collectionid_seq OWNED BY collections.collectionid;


--
-- Name: collections_descriptionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE collections_descriptionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.collections_descriptionid_seq OWNER TO mushu;

--
-- Name: collections_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collections_descriptionid_seq OWNED BY collections.descriptionid;


--
-- Name: descriptions; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE descriptions (
    descriptionid integer NOT NULL,
    author integer NOT NULL,
    headline character varying(255) NOT NULL,
    description text NOT NULL,
    creation timestamp without time zone DEFAULT now() NOT NULL,
    deletion timestamp without time zone
);


ALTER TABLE public.descriptions OWNER TO mushu;

--
-- Name: descriptions_author_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE descriptions_author_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.descriptions_author_seq OWNER TO mushu;

--
-- Name: descriptions_author_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE descriptions_author_seq OWNED BY descriptions.author;


--
-- Name: descriptions_descriptionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE descriptions_descriptionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.descriptions_descriptionid_seq OWNER TO mushu;

--
-- Name: descriptions_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE descriptions_descriptionid_seq OWNED BY descriptions.descriptionid;


--
-- Name: discussions; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE discussions (
    discussionid integer NOT NULL,
    collectionid integer NOT NULL,
    deadline timestamp without time zone,
    resultid integer
);


ALTER TABLE public.discussions OWNER TO mushu;

--
-- Name: discussions_collectionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE discussions_collectionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.discussions_collectionid_seq OWNER TO mushu;

--
-- Name: discussions_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE discussions_collectionid_seq OWNED BY discussions.collectionid;


--
-- Name: discussions_discussionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE discussions_discussionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.discussions_discussionid_seq OWNER TO mushu;

--
-- Name: discussions_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE discussions_discussionid_seq OWNED BY discussions.discussionid;


--
-- Name: participants; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE participants (
    discussionid integer NOT NULL,
    userid integer NOT NULL
);


ALTER TABLE public.participants OWNER TO mushu;

--
-- Name: participants_discussionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE participants_discussionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.participants_discussionid_seq OWNER TO mushu;

--
-- Name: participants_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE participants_discussionid_seq OWNED BY participants.discussionid;


--
-- Name: participants_userid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE participants_userid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.participants_userid_seq OWNER TO mushu;

--
-- Name: participants_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE participants_userid_seq OWNED BY participants.userid;


--
-- Name: relations; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE relations (
    relationid integer NOT NULL,
    descriptionid integer NOT NULL,
    discussionid integer NOT NULL,
    source integer NOT NULL,
    target integer NOT NULL
);


ALTER TABLE public.relations OWNER TO mushu;

--
-- Name: relations_descriptionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE relations_descriptionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.relations_descriptionid_seq OWNER TO mushu;

--
-- Name: relations_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_descriptionid_seq OWNED BY relations.descriptionid;


--
-- Name: relations_discussionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE relations_discussionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.relations_discussionid_seq OWNER TO mushu;

--
-- Name: relations_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_discussionid_seq OWNED BY relations.discussionid;


--
-- Name: relations_relationid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE relations_relationid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.relations_relationid_seq OWNER TO mushu;

--
-- Name: relations_relationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_relationid_seq OWNED BY relations.relationid;


--
-- Name: relations_source_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE relations_source_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.relations_source_seq OWNER TO mushu;

--
-- Name: relations_source_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_source_seq OWNED BY relations.source;


--
-- Name: relations_target_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE relations_target_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.relations_target_seq OWNER TO mushu;

--
-- Name: relations_target_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_target_seq OWNED BY relations.target;


--
-- Name: results; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE results (
    resultid integer NOT NULL
);


ALTER TABLE public.results OWNER TO mushu;

--
-- Name: results_resultid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE results_resultid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.results_resultid_seq OWNER TO mushu;

--
-- Name: results_resultid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE results_resultid_seq OWNED BY results.resultid;


--
-- Name: users; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE users (
    userid integer NOT NULL,
    username character varying(255) NOT NULL,
    hash character varying(128) NOT NULL,
    salt character varying(255) NOT NULL,
    creationtime timestamp without time zone DEFAULT now() NOT NULL,
    lastlogin timestamp without time zone DEFAULT now() NOT NULL,
    isadmin boolean DEFAULT false NOT NULL,
    profile integer,
    sessionkey character varying(255)
);


ALTER TABLE public.users OWNER TO mushu;

--
-- Name: users_userid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE users_userid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_userid_seq OWNER TO mushu;

--
-- Name: users_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE users_userid_seq OWNED BY users.userid;


--
-- Name: voters; Type: TABLE; Schema: public; Owner: mushu; Tablespace: 
--

CREATE TABLE voters (
    resultid integer NOT NULL,
    userid integer NOT NULL,
    voted boolean DEFAULT false NOT NULL
);


ALTER TABLE public.voters OWNER TO mushu;

--
-- Name: voters_resultid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE voters_resultid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.voters_resultid_seq OWNER TO mushu;

--
-- Name: voters_resultid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE voters_resultid_seq OWNED BY voters.resultid;


--
-- Name: voters_userid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE voters_userid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.voters_userid_seq OWNER TO mushu;

--
-- Name: voters_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE voters_userid_seq OWNED BY voters.userid;


--
-- Name: articleid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles ALTER COLUMN articleid SET DEFAULT nextval('articles_articleid_seq'::regclass);


--
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles ALTER COLUMN descriptionid SET DEFAULT nextval('articles_descriptionid_seq'::regclass);


--
-- Name: parent; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children ALTER COLUMN parent SET DEFAULT nextval('children_parent_seq'::regclass);


--
-- Name: child; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children ALTER COLUMN child SET DEFAULT nextval('children_child_seq'::regclass);


--
-- Name: resultid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices ALTER COLUMN resultid SET DEFAULT nextval('choices_resultid_seq'::regclass);


--
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices ALTER COLUMN collectionid SET DEFAULT nextval('choices_collectionid_seq'::regclass);


--
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles ALTER COLUMN collectionid SET DEFAULT nextval('collectedarticles_collectionid_seq'::regclass);


--
-- Name: articleid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles ALTER COLUMN articleid SET DEFAULT nextval('collectedarticles_articleid_seq'::regclass);


--
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections ALTER COLUMN collectionid SET DEFAULT nextval('collections_collectionid_seq'::regclass);


--
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections ALTER COLUMN descriptionid SET DEFAULT nextval('collections_descriptionid_seq'::regclass);


--
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions ALTER COLUMN descriptionid SET DEFAULT nextval('descriptions_descriptionid_seq'::regclass);


--
-- Name: author; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions ALTER COLUMN author SET DEFAULT nextval('descriptions_author_seq'::regclass);


--
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions ALTER COLUMN discussionid SET DEFAULT nextval('discussions_discussionid_seq'::regclass);


--
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions ALTER COLUMN collectionid SET DEFAULT nextval('discussions_collectionid_seq'::regclass);


--
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants ALTER COLUMN discussionid SET DEFAULT nextval('participants_discussionid_seq'::regclass);


--
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants ALTER COLUMN userid SET DEFAULT nextval('participants_userid_seq'::regclass);


--
-- Name: relationid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN relationid SET DEFAULT nextval('relations_relationid_seq'::regclass);


--
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN descriptionid SET DEFAULT nextval('relations_descriptionid_seq'::regclass);


--
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN discussionid SET DEFAULT nextval('relations_discussionid_seq'::regclass);


--
-- Name: source; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN source SET DEFAULT nextval('relations_source_seq'::regclass);


--
-- Name: target; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN target SET DEFAULT nextval('relations_target_seq'::regclass);


--
-- Name: resultid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY results ALTER COLUMN resultid SET DEFAULT nextval('results_resultid_seq'::regclass);


--
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY users ALTER COLUMN userid SET DEFAULT nextval('users_userid_seq'::regclass);


--
-- Name: resultid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters ALTER COLUMN resultid SET DEFAULT nextval('voters_resultid_seq'::regclass);


--
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters ALTER COLUMN userid SET DEFAULT nextval('voters_userid_seq'::regclass);


--
-- Data for Name: articles; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY articles (articleid, descriptionid, content) FROM stdin;
\.


--
-- Name: articles_articleid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('articles_articleid_seq', 1, false);


--
-- Name: articles_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('articles_descriptionid_seq', 1, false);


--
-- Data for Name: children; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY children (parent, child) FROM stdin;
\.


--
-- Name: children_child_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('children_child_seq', 1, false);


--
-- Name: children_parent_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('children_parent_seq', 1, false);


--
-- Data for Name: choices; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY choices (resultid, collectionid, votes) FROM stdin;
\.


--
-- Name: choices_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('choices_collectionid_seq', 1, false);


--
-- Name: choices_resultid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('choices_resultid_seq', 1, false);


--
-- Data for Name: collectedarticles; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY collectedarticles (collectionid, articleid, pos_x, pos_y, accepted, condition, customcondition) FROM stdin;
\.


--
-- Name: collectedarticles_articleid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collectedarticles_articleid_seq', 1, false);


--
-- Name: collectedarticles_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collectedarticles_collectionid_seq', 1, false);


--
-- Data for Name: collections; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY collections (collectionid, descriptionid) FROM stdin;
\.


--
-- Name: collections_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collections_collectionid_seq', 1, false);


--
-- Name: collections_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collections_descriptionid_seq', 1, false);


--
-- Data for Name: descriptions; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY descriptions (descriptionid, author, headline, description, creation, deletion) FROM stdin;
\.


--
-- Name: descriptions_author_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('descriptions_author_seq', 1, false);


--
-- Name: descriptions_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('descriptions_descriptionid_seq', 1, false);


--
-- Data for Name: discussions; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY discussions (discussionid, collectionid, deadline, resultid) FROM stdin;
\.


--
-- Name: discussions_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('discussions_collectionid_seq', 1, false);


--
-- Name: discussions_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('discussions_discussionid_seq', 1, false);


--
-- Data for Name: participants; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY participants (discussionid, userid) FROM stdin;
\.


--
-- Name: participants_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('participants_discussionid_seq', 1, false);


--
-- Name: participants_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('participants_userid_seq', 1, false);


--
-- Data for Name: relations; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY relations (relationid, descriptionid, discussionid, source, target) FROM stdin;
\.


--
-- Name: relations_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_descriptionid_seq', 1, false);


--
-- Name: relations_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_discussionid_seq', 1, false);


--
-- Name: relations_relationid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_relationid_seq', 1, false);


--
-- Name: relations_source_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_source_seq', 1, false);


--
-- Name: relations_target_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_target_seq', 1, false);


--
-- Data for Name: results; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY results (resultid) FROM stdin;
\.


--
-- Name: results_resultid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('results_resultid_seq', 1, false);


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY users (userid, username, hash, salt, creationtime, lastlogin, isadmin, profile, sessionkey) FROM stdin;
1	foo	50c9387054e4afec024be1881ed61dd57f2c349910b2e0af0795ae48c14b8992f39125588b6ba8ec4f4c4580b1d867a607419d2308cc6843450b7ff660a8844d	bfzsxmqxhbzhd{fc	2013-07-08 14:41:12.644502	2013-07-08 14:41:12.644502	t	\N	\N
\.


--
-- Name: users_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('users_userid_seq', 1, true);


--
-- Data for Name: voters; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY voters (resultid, userid, voted) FROM stdin;
\.


--
-- Name: voters_resultid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('voters_resultid_seq', 1, false);


--
-- Name: voters_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('voters_userid_seq', 1, false);


--
-- Name: articles_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_primarykey PRIMARY KEY (articleid);


--
-- Name: collections_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY collections
    ADD CONSTRAINT collections_primarykey PRIMARY KEY (collectionid);


--
-- Name: descriptions_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY descriptions
    ADD CONSTRAINT descriptions_primarykey PRIMARY KEY (descriptionid);


--
-- Name: discussions_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY discussions
    ADD CONSTRAINT discussions_primarykey PRIMARY KEY (discussionid);


--
-- Name: relations_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_primarykey PRIMARY KEY (relationid);


--
-- Name: results_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY results
    ADD CONSTRAINT results_primarykey PRIMARY KEY (resultid);


--
-- Name: users_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_primarykey PRIMARY KEY (userid);


--
-- Name: users_username_unique; Type: CONSTRAINT; Schema: public; Owner: mushu; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_username_unique UNIQUE (username);


--
-- Name: articles_descriptionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_descriptionid FOREIGN KEY (descriptionid) REFERENCES descriptions(descriptionid) MATCH FULL ON DELETE CASCADE;


--
-- Name: children_child; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children
    ADD CONSTRAINT children_child FOREIGN KEY (child) REFERENCES articles(articleid) MATCH FULL;


--
-- Name: children_parent; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children
    ADD CONSTRAINT children_parent FOREIGN KEY (parent) REFERENCES articles(articleid) MATCH FULL;


--
-- Name: choices_collectionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices
    ADD CONSTRAINT choices_collectionid FOREIGN KEY (collectionid) REFERENCES collections(collectionid) MATCH FULL ON DELETE CASCADE;


--
-- Name: choices_resultid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices
    ADD CONSTRAINT choices_resultid FOREIGN KEY (resultid) REFERENCES results(resultid) MATCH FULL ON DELETE CASCADE;


--
-- Name: collectedarticles_articleid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles
    ADD CONSTRAINT collectedarticles_articleid FOREIGN KEY (articleid) REFERENCES articles(articleid) MATCH FULL;


--
-- Name: collectedarticles_collectionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles
    ADD CONSTRAINT collectedarticles_collectionid FOREIGN KEY (collectionid) REFERENCES collections(collectionid) MATCH FULL;


--
-- Name: collections_descriptionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections
    ADD CONSTRAINT collections_descriptionid FOREIGN KEY (descriptionid) REFERENCES descriptions(descriptionid) MATCH FULL ON DELETE CASCADE;


--
-- Name: descriptions_author; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions
    ADD CONSTRAINT descriptions_author FOREIGN KEY (author) REFERENCES users(userid) MATCH FULL;


--
-- Name: discussions_collectionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions
    ADD CONSTRAINT discussions_collectionid FOREIGN KEY (collectionid) REFERENCES collections(collectionid) MATCH FULL ON DELETE CASCADE;


--
-- Name: discussions_result; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions
    ADD CONSTRAINT discussions_result FOREIGN KEY (resultid) REFERENCES results(resultid) MATCH FULL;


--
-- Name: participants_discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants
    ADD CONSTRAINT participants_discussionid FOREIGN KEY (discussionid) REFERENCES discussions(discussionid) MATCH FULL;


--
-- Name: participants_userid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants
    ADD CONSTRAINT participants_userid FOREIGN KEY (userid) REFERENCES users(userid) MATCH FULL;


--
-- Name: relations_description; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_description FOREIGN KEY (descriptionid) REFERENCES descriptions(descriptionid) MATCH FULL;


--
-- Name: relations_discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_discussionid FOREIGN KEY (discussionid) REFERENCES discussions(discussionid) MATCH FULL;


--
-- Name: relations_source; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_source FOREIGN KEY (source) REFERENCES articles(articleid) MATCH FULL ON DELETE CASCADE;


--
-- Name: relations_target; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_target FOREIGN KEY (target) REFERENCES articles(articleid) MATCH FULL ON DELETE CASCADE;


--
-- Name: users_profile; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_profile FOREIGN KEY (profile) REFERENCES articles(articleid) MATCH FULL;


--
-- Name: voters_resultid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters
    ADD CONSTRAINT voters_resultid FOREIGN KEY (resultid) REFERENCES results(resultid) MATCH FULL ON DELETE CASCADE;


--
-- Name: voters_userid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters
    ADD CONSTRAINT voters_userid FOREIGN KEY (userid) REFERENCES users(userid) MATCH FULL ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

