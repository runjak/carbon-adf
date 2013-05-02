--
-- PostgreSQL database dump
--

-- Dumped from database version 9.1.9
-- Dumped by pg_dump version 9.1.9
-- Started on 2013-05-03 01:18:55 CEST

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- TOC entry 2108 (class 1262 OID 16507)
-- Dependencies: 2107
-- Name: openbrain; Type: COMMENT; Schema: -; Owner: mushu
--

COMMENT ON DATABASE openbrain IS 'The openbrain development database. It uses the schema from the design document with the date of 2013-04-29.';


--
-- TOC entry 201 (class 3079 OID 11681)
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- TOC entry 2110 (class 0 OID 0)
-- Dependencies: 201
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_with_oids = true;

--
-- TOC entry 168 (class 1259 OID 17401)
-- Dependencies: 5
-- Name: articles; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE articles (
    articleid integer NOT NULL,
    descriptionid integer NOT NULL,
    content text NOT NULL
);


ALTER TABLE public.articles OWNER TO mushu;

--
-- TOC entry 166 (class 1259 OID 17397)
-- Dependencies: 5 168
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
-- TOC entry 2111 (class 0 OID 0)
-- Dependencies: 166
-- Name: articles_articleid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE articles_articleid_seq OWNED BY articles.articleid;


--
-- TOC entry 167 (class 1259 OID 17399)
-- Dependencies: 168 5
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
-- TOC entry 2112 (class 0 OID 0)
-- Dependencies: 167
-- Name: articles_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE articles_descriptionid_seq OWNED BY articles.descriptionid;


--
-- TOC entry 200 (class 1259 OID 17513)
-- Dependencies: 5
-- Name: children; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE children (
    parent integer NOT NULL,
    child integer NOT NULL
);


ALTER TABLE public.children OWNER TO mushu;

--
-- TOC entry 199 (class 1259 OID 17511)
-- Dependencies: 200 5
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
-- TOC entry 2113 (class 0 OID 0)
-- Dependencies: 199
-- Name: children_child_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE children_child_seq OWNED BY children.child;


--
-- TOC entry 198 (class 1259 OID 17509)
-- Dependencies: 5 200
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
-- TOC entry 2114 (class 0 OID 0)
-- Dependencies: 198
-- Name: children_parent_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE children_parent_seq OWNED BY children.parent;


--
-- TOC entry 194 (class 1259 OID 17493)
-- Dependencies: 2019 5
-- Name: choices; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE choices (
    resultid integer NOT NULL,
    collectionid integer NOT NULL,
    votes integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.choices OWNER TO mushu;

--
-- TOC entry 193 (class 1259 OID 17491)
-- Dependencies: 5 194
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
-- TOC entry 2115 (class 0 OID 0)
-- Dependencies: 193
-- Name: choices_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE choices_collectionid_seq OWNED BY choices.collectionid;


--
-- TOC entry 192 (class 1259 OID 17489)
-- Dependencies: 5 194
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
-- TOC entry 2116 (class 0 OID 0)
-- Dependencies: 192
-- Name: choices_resultid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE choices_resultid_seq OWNED BY choices.resultid;


--
-- TOC entry 174 (class 1259 OID 17426)
-- Dependencies: 5
-- Name: collectedarticles; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE collectedarticles (
    collectionid integer NOT NULL,
    articleid integer NOT NULL
);


ALTER TABLE public.collectedarticles OWNER TO mushu;

--
-- TOC entry 173 (class 1259 OID 17424)
-- Dependencies: 5 174
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
-- TOC entry 2117 (class 0 OID 0)
-- Dependencies: 173
-- Name: collectedarticles_articleid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collectedarticles_articleid_seq OWNED BY collectedarticles.articleid;


--
-- TOC entry 172 (class 1259 OID 17422)
-- Dependencies: 174 5
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
-- TOC entry 2118 (class 0 OID 0)
-- Dependencies: 172
-- Name: collectedarticles_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collectedarticles_collectionid_seq OWNED BY collectedarticles.collectionid;


--
-- TOC entry 171 (class 1259 OID 17415)
-- Dependencies: 5
-- Name: collections; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE collections (
    collectionid integer NOT NULL,
    descriptionid integer NOT NULL
);


ALTER TABLE public.collections OWNER TO mushu;

--
-- TOC entry 169 (class 1259 OID 17411)
-- Dependencies: 5 171
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
-- TOC entry 2119 (class 0 OID 0)
-- Dependencies: 169
-- Name: collections_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collections_collectionid_seq OWNED BY collections.collectionid;


--
-- TOC entry 170 (class 1259 OID 17413)
-- Dependencies: 171 5
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
-- TOC entry 2120 (class 0 OID 0)
-- Dependencies: 170
-- Name: collections_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE collections_descriptionid_seq OWNED BY collections.descriptionid;


--
-- TOC entry 165 (class 1259 OID 17386)
-- Dependencies: 1997 5
-- Name: descriptions; Type: TABLE; Schema: public; Owner: mushu
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
-- TOC entry 164 (class 1259 OID 17384)
-- Dependencies: 165 5
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
-- TOC entry 2121 (class 0 OID 0)
-- Dependencies: 164
-- Name: descriptions_author_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE descriptions_author_seq OWNED BY descriptions.author;


--
-- TOC entry 163 (class 1259 OID 17382)
-- Dependencies: 5 165
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
-- TOC entry 2122 (class 0 OID 0)
-- Dependencies: 163
-- Name: descriptions_descriptionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE descriptions_descriptionid_seq OWNED BY descriptions.descriptionid;


--
-- TOC entry 177 (class 1259 OID 17435)
-- Dependencies: 5
-- Name: discussions; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE discussions (
    discussionid integer NOT NULL,
    collectionid integer NOT NULL,
    deadline timestamp without time zone NOT NULL,
    resultid integer
);


ALTER TABLE public.discussions OWNER TO mushu;

--
-- TOC entry 176 (class 1259 OID 17433)
-- Dependencies: 177 5
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
-- TOC entry 2123 (class 0 OID 0)
-- Dependencies: 176
-- Name: discussions_collectionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE discussions_collectionid_seq OWNED BY discussions.collectionid;


--
-- TOC entry 175 (class 1259 OID 17431)
-- Dependencies: 5 177
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
-- TOC entry 2124 (class 0 OID 0)
-- Dependencies: 175
-- Name: discussions_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE discussions_discussionid_seq OWNED BY discussions.discussionid;


--
-- TOC entry 180 (class 1259 OID 17446)
-- Dependencies: 5
-- Name: participants; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE participants (
    discussionid integer NOT NULL,
    userid integer NOT NULL
);


ALTER TABLE public.participants OWNER TO mushu;

--
-- TOC entry 178 (class 1259 OID 17442)
-- Dependencies: 5 180
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
-- TOC entry 2125 (class 0 OID 0)
-- Dependencies: 178
-- Name: participants_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE participants_discussionid_seq OWNED BY participants.discussionid;


--
-- TOC entry 179 (class 1259 OID 17444)
-- Dependencies: 5 180
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
-- TOC entry 2126 (class 0 OID 0)
-- Dependencies: 179
-- Name: participants_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE participants_userid_seq OWNED BY participants.userid;


--
-- TOC entry 185 (class 1259 OID 17459)
-- Dependencies: 5
-- Name: relations; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE relations (
    relationid integer NOT NULL,
    "descriptionId" integer NOT NULL,
    source integer NOT NULL,
    target integer NOT NULL,
    type character varying(255) NOT NULL
);


ALTER TABLE public.relations OWNER TO mushu;

--
-- TOC entry 182 (class 1259 OID 17453)
-- Dependencies: 185 5
-- Name: relations_descriptionId_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE "relations_descriptionId_seq"
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public."relations_descriptionId_seq" OWNER TO mushu;

--
-- TOC entry 2127 (class 0 OID 0)
-- Dependencies: 182
-- Name: relations_descriptionId_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE "relations_descriptionId_seq" OWNED BY relations."descriptionId";


--
-- TOC entry 181 (class 1259 OID 17451)
-- Dependencies: 185 5
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
-- TOC entry 2128 (class 0 OID 0)
-- Dependencies: 181
-- Name: relations_relationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_relationid_seq OWNED BY relations.relationid;


--
-- TOC entry 183 (class 1259 OID 17455)
-- Dependencies: 185 5
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
-- TOC entry 2129 (class 0 OID 0)
-- Dependencies: 183
-- Name: relations_source_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_source_seq OWNED BY relations.source;


--
-- TOC entry 184 (class 1259 OID 17457)
-- Dependencies: 185 5
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
-- TOC entry 2130 (class 0 OID 0)
-- Dependencies: 184
-- Name: relations_target_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE relations_target_seq OWNED BY relations.target;


--
-- TOC entry 191 (class 1259 OID 17483)
-- Dependencies: 5
-- Name: results; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE results (
    resultid integer NOT NULL
);


ALTER TABLE public.results OWNER TO mushu;

--
-- TOC entry 190 (class 1259 OID 17481)
-- Dependencies: 5 191
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
-- TOC entry 2131 (class 0 OID 0)
-- Dependencies: 190
-- Name: results_resultid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE results_resultid_seq OWNED BY results.resultid;


--
-- TOC entry 162 (class 1259 OID 17368)
-- Dependencies: 1992 1993 1994 5
-- Name: users; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE users (
    userid integer NOT NULL,
    username character varying(255) NOT NULL,
    hash character varying(32) NOT NULL,
    salt character varying(255) NOT NULL,
    creationtime timestamp without time zone DEFAULT now() NOT NULL,
    lastlogin timestamp without time zone DEFAULT now() NOT NULL,
    isadmin boolean DEFAULT false NOT NULL,
    profile integer,
    sessionkey character varying(255)
);


ALTER TABLE public.users OWNER TO mushu;

--
-- TOC entry 161 (class 1259 OID 17366)
-- Dependencies: 162 5
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
-- TOC entry 2132 (class 0 OID 0)
-- Dependencies: 161
-- Name: users_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE users_userid_seq OWNED BY users.userid;


--
-- TOC entry 197 (class 1259 OID 17503)
-- Dependencies: 2022 5
-- Name: voters; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE voters (
    resultid integer NOT NULL,
    userid integer NOT NULL,
    voted boolean DEFAULT false NOT NULL
);


ALTER TABLE public.voters OWNER TO mushu;

--
-- TOC entry 195 (class 1259 OID 17499)
-- Dependencies: 197 5
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
-- TOC entry 2133 (class 0 OID 0)
-- Dependencies: 195
-- Name: voters_resultid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE voters_resultid_seq OWNED BY voters.resultid;


--
-- TOC entry 196 (class 1259 OID 17501)
-- Dependencies: 5 197
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
-- TOC entry 2134 (class 0 OID 0)
-- Dependencies: 196
-- Name: voters_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE voters_userid_seq OWNED BY voters.userid;


--
-- TOC entry 189 (class 1259 OID 17474)
-- Dependencies: 2015 5
-- Name: weights; Type: TABLE; Schema: public; Owner: mushu
--

CREATE TABLE weights (
    discussionid integer NOT NULL,
    userid integer NOT NULL,
    relationid integer NOT NULL,
    weight integer DEFAULT 0 NOT NULL
);


ALTER TABLE public.weights OWNER TO mushu;

--
-- TOC entry 186 (class 1259 OID 17468)
-- Dependencies: 5 189
-- Name: weights_discussionid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE weights_discussionid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.weights_discussionid_seq OWNER TO mushu;

--
-- TOC entry 2135 (class 0 OID 0)
-- Dependencies: 186
-- Name: weights_discussionid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE weights_discussionid_seq OWNED BY weights.discussionid;


--
-- TOC entry 188 (class 1259 OID 17472)
-- Dependencies: 5 189
-- Name: weights_relationid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE weights_relationid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.weights_relationid_seq OWNER TO mushu;

--
-- TOC entry 2136 (class 0 OID 0)
-- Dependencies: 188
-- Name: weights_relationid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE weights_relationid_seq OWNED BY weights.relationid;


--
-- TOC entry 187 (class 1259 OID 17470)
-- Dependencies: 5 189
-- Name: weights_userid_seq; Type: SEQUENCE; Schema: public; Owner: mushu
--

CREATE SEQUENCE weights_userid_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.weights_userid_seq OWNER TO mushu;

--
-- TOC entry 2137 (class 0 OID 0)
-- Dependencies: 187
-- Name: weights_userid_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: mushu
--

ALTER SEQUENCE weights_userid_seq OWNED BY weights.userid;


--
-- TOC entry 1998 (class 2604 OID 17404)
-- Dependencies: 166 168 168
-- Name: articleid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles ALTER COLUMN articleid SET DEFAULT nextval('articles_articleid_seq'::regclass);


--
-- TOC entry 1999 (class 2604 OID 17405)
-- Dependencies: 168 167 168
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles ALTER COLUMN descriptionid SET DEFAULT nextval('articles_descriptionid_seq'::regclass);


--
-- TOC entry 2023 (class 2604 OID 17516)
-- Dependencies: 200 198 200
-- Name: parent; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children ALTER COLUMN parent SET DEFAULT nextval('children_parent_seq'::regclass);


--
-- TOC entry 2024 (class 2604 OID 17517)
-- Dependencies: 199 200 200
-- Name: child; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children ALTER COLUMN child SET DEFAULT nextval('children_child_seq'::regclass);


--
-- TOC entry 2017 (class 2604 OID 17496)
-- Dependencies: 194 192 194
-- Name: resultid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices ALTER COLUMN resultid SET DEFAULT nextval('choices_resultid_seq'::regclass);


--
-- TOC entry 2018 (class 2604 OID 17497)
-- Dependencies: 193 194 194
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices ALTER COLUMN collectionid SET DEFAULT nextval('choices_collectionid_seq'::regclass);


--
-- TOC entry 2002 (class 2604 OID 17429)
-- Dependencies: 174 172 174
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles ALTER COLUMN collectionid SET DEFAULT nextval('collectedarticles_collectionid_seq'::regclass);


--
-- TOC entry 2003 (class 2604 OID 17430)
-- Dependencies: 174 173 174
-- Name: articleid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles ALTER COLUMN articleid SET DEFAULT nextval('collectedarticles_articleid_seq'::regclass);


--
-- TOC entry 2000 (class 2604 OID 17418)
-- Dependencies: 171 169 171
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections ALTER COLUMN collectionid SET DEFAULT nextval('collections_collectionid_seq'::regclass);


--
-- TOC entry 2001 (class 2604 OID 17419)
-- Dependencies: 171 170 171
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections ALTER COLUMN descriptionid SET DEFAULT nextval('collections_descriptionid_seq'::regclass);


--
-- TOC entry 1995 (class 2604 OID 17389)
-- Dependencies: 165 163 165
-- Name: descriptionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions ALTER COLUMN descriptionid SET DEFAULT nextval('descriptions_descriptionid_seq'::regclass);


--
-- TOC entry 1996 (class 2604 OID 17390)
-- Dependencies: 164 165 165
-- Name: author; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions ALTER COLUMN author SET DEFAULT nextval('descriptions_author_seq'::regclass);


--
-- TOC entry 2004 (class 2604 OID 17438)
-- Dependencies: 175 177 177
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions ALTER COLUMN discussionid SET DEFAULT nextval('discussions_discussionid_seq'::regclass);


--
-- TOC entry 2005 (class 2604 OID 17439)
-- Dependencies: 176 177 177
-- Name: collectionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions ALTER COLUMN collectionid SET DEFAULT nextval('discussions_collectionid_seq'::regclass);


--
-- TOC entry 2006 (class 2604 OID 17449)
-- Dependencies: 178 180 180
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants ALTER COLUMN discussionid SET DEFAULT nextval('participants_discussionid_seq'::regclass);


--
-- TOC entry 2007 (class 2604 OID 17450)
-- Dependencies: 179 180 180
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants ALTER COLUMN userid SET DEFAULT nextval('participants_userid_seq'::regclass);


--
-- TOC entry 2008 (class 2604 OID 17462)
-- Dependencies: 181 185 185
-- Name: relationid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN relationid SET DEFAULT nextval('relations_relationid_seq'::regclass);


--
-- TOC entry 2009 (class 2604 OID 17463)
-- Dependencies: 182 185 185
-- Name: descriptionId; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN "descriptionId" SET DEFAULT nextval('"relations_descriptionId_seq"'::regclass);


--
-- TOC entry 2010 (class 2604 OID 17464)
-- Dependencies: 183 185 185
-- Name: source; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN source SET DEFAULT nextval('relations_source_seq'::regclass);


--
-- TOC entry 2011 (class 2604 OID 17465)
-- Dependencies: 184 185 185
-- Name: target; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations ALTER COLUMN target SET DEFAULT nextval('relations_target_seq'::regclass);


--
-- TOC entry 2016 (class 2604 OID 17486)
-- Dependencies: 191 190 191
-- Name: resultid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY results ALTER COLUMN resultid SET DEFAULT nextval('results_resultid_seq'::regclass);


--
-- TOC entry 1991 (class 2604 OID 17371)
-- Dependencies: 162 161 162
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY users ALTER COLUMN userid SET DEFAULT nextval('users_userid_seq'::regclass);


--
-- TOC entry 2020 (class 2604 OID 17506)
-- Dependencies: 197 195 197
-- Name: resultid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters ALTER COLUMN resultid SET DEFAULT nextval('voters_resultid_seq'::regclass);


--
-- TOC entry 2021 (class 2604 OID 17507)
-- Dependencies: 196 197 197
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters ALTER COLUMN userid SET DEFAULT nextval('voters_userid_seq'::regclass);


--
-- TOC entry 2012 (class 2604 OID 17477)
-- Dependencies: 189 186 189
-- Name: discussionid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY weights ALTER COLUMN discussionid SET DEFAULT nextval('weights_discussionid_seq'::regclass);


--
-- TOC entry 2013 (class 2604 OID 17478)
-- Dependencies: 187 189 189
-- Name: userid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY weights ALTER COLUMN userid SET DEFAULT nextval('weights_userid_seq'::regclass);


--
-- TOC entry 2014 (class 2604 OID 17479)
-- Dependencies: 189 188 189
-- Name: relationid; Type: DEFAULT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY weights ALTER COLUMN relationid SET DEFAULT nextval('weights_relationid_seq'::regclass);


--
-- TOC entry 2070 (class 0 OID 17401)
-- Dependencies: 168 2103
-- Data for Name: articles; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY articles (articleid, descriptionid, content) FROM stdin;
\.


--
-- TOC entry 2138 (class 0 OID 0)
-- Dependencies: 166
-- Name: articles_articleid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('articles_articleid_seq', 1, false);


--
-- TOC entry 2139 (class 0 OID 0)
-- Dependencies: 167
-- Name: articles_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('articles_descriptionid_seq', 1, false);


--
-- TOC entry 2102 (class 0 OID 17513)
-- Dependencies: 200 2103
-- Data for Name: children; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY children (parent, child) FROM stdin;
\.


--
-- TOC entry 2140 (class 0 OID 0)
-- Dependencies: 199
-- Name: children_child_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('children_child_seq', 1, false);


--
-- TOC entry 2141 (class 0 OID 0)
-- Dependencies: 198
-- Name: children_parent_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('children_parent_seq', 1, false);


--
-- TOC entry 2096 (class 0 OID 17493)
-- Dependencies: 194 2103
-- Data for Name: choices; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY choices (resultid, collectionid, votes) FROM stdin;
\.


--
-- TOC entry 2142 (class 0 OID 0)
-- Dependencies: 193
-- Name: choices_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('choices_collectionid_seq', 1, false);


--
-- TOC entry 2143 (class 0 OID 0)
-- Dependencies: 192
-- Name: choices_resultid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('choices_resultid_seq', 1, false);


--
-- TOC entry 2076 (class 0 OID 17426)
-- Dependencies: 174 2103
-- Data for Name: collectedarticles; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY collectedarticles (collectionid, articleid) FROM stdin;
\.


--
-- TOC entry 2144 (class 0 OID 0)
-- Dependencies: 173
-- Name: collectedarticles_articleid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collectedarticles_articleid_seq', 1, false);


--
-- TOC entry 2145 (class 0 OID 0)
-- Dependencies: 172
-- Name: collectedarticles_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collectedarticles_collectionid_seq', 1, false);


--
-- TOC entry 2073 (class 0 OID 17415)
-- Dependencies: 171 2103
-- Data for Name: collections; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY collections (collectionid, descriptionid) FROM stdin;
\.


--
-- TOC entry 2146 (class 0 OID 0)
-- Dependencies: 169
-- Name: collections_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collections_collectionid_seq', 1, false);


--
-- TOC entry 2147 (class 0 OID 0)
-- Dependencies: 170
-- Name: collections_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('collections_descriptionid_seq', 1, false);


--
-- TOC entry 2067 (class 0 OID 17386)
-- Dependencies: 165 2103
-- Data for Name: descriptions; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY descriptions (descriptionid, author, headline, description, creation, deletion) FROM stdin;
\.


--
-- TOC entry 2148 (class 0 OID 0)
-- Dependencies: 164
-- Name: descriptions_author_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('descriptions_author_seq', 1, false);


--
-- TOC entry 2149 (class 0 OID 0)
-- Dependencies: 163
-- Name: descriptions_descriptionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('descriptions_descriptionid_seq', 1, false);


--
-- TOC entry 2079 (class 0 OID 17435)
-- Dependencies: 177 2103
-- Data for Name: discussions; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY discussions (discussionid, collectionid, deadline, resultid) FROM stdin;
\.


--
-- TOC entry 2150 (class 0 OID 0)
-- Dependencies: 176
-- Name: discussions_collectionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('discussions_collectionid_seq', 1, false);


--
-- TOC entry 2151 (class 0 OID 0)
-- Dependencies: 175
-- Name: discussions_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('discussions_discussionid_seq', 1, false);


--
-- TOC entry 2082 (class 0 OID 17446)
-- Dependencies: 180 2103
-- Data for Name: participants; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY participants (discussionid, userid) FROM stdin;
\.


--
-- TOC entry 2152 (class 0 OID 0)
-- Dependencies: 178
-- Name: participants_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('participants_discussionid_seq', 1, false);


--
-- TOC entry 2153 (class 0 OID 0)
-- Dependencies: 179
-- Name: participants_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('participants_userid_seq', 1, false);


--
-- TOC entry 2087 (class 0 OID 17459)
-- Dependencies: 185 2103
-- Data for Name: relations; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY relations (relationid, "descriptionId", source, target, type) FROM stdin;
\.


--
-- TOC entry 2154 (class 0 OID 0)
-- Dependencies: 182
-- Name: relations_descriptionId_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('"relations_descriptionId_seq"', 1, false);


--
-- TOC entry 2155 (class 0 OID 0)
-- Dependencies: 181
-- Name: relations_relationid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_relationid_seq', 1, false);


--
-- TOC entry 2156 (class 0 OID 0)
-- Dependencies: 183
-- Name: relations_source_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_source_seq', 1, false);


--
-- TOC entry 2157 (class 0 OID 0)
-- Dependencies: 184
-- Name: relations_target_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('relations_target_seq', 1, false);


--
-- TOC entry 2093 (class 0 OID 17483)
-- Dependencies: 191 2103
-- Data for Name: results; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY results (resultid) FROM stdin;
\.


--
-- TOC entry 2158 (class 0 OID 0)
-- Dependencies: 190
-- Name: results_resultid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('results_resultid_seq', 1, false);


--
-- TOC entry 2064 (class 0 OID 17368)
-- Dependencies: 162 2103
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY users (userid, username, hash, salt, creationtime, lastlogin, isadmin, profile, sessionkey) FROM stdin;
\.


--
-- TOC entry 2159 (class 0 OID 0)
-- Dependencies: 161
-- Name: users_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('users_userid_seq', 1, false);


--
-- TOC entry 2099 (class 0 OID 17503)
-- Dependencies: 197 2103
-- Data for Name: voters; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY voters (resultid, userid, voted) FROM stdin;
\.


--
-- TOC entry 2160 (class 0 OID 0)
-- Dependencies: 195
-- Name: voters_resultid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('voters_resultid_seq', 1, false);


--
-- TOC entry 2161 (class 0 OID 0)
-- Dependencies: 196
-- Name: voters_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('voters_userid_seq', 1, false);


--
-- TOC entry 2091 (class 0 OID 17474)
-- Dependencies: 189 2103
-- Data for Name: weights; Type: TABLE DATA; Schema: public; Owner: mushu
--

COPY weights (discussionid, userid, relationid, weight) FROM stdin;
\.


--
-- TOC entry 2162 (class 0 OID 0)
-- Dependencies: 186
-- Name: weights_discussionid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('weights_discussionid_seq', 1, false);


--
-- TOC entry 2163 (class 0 OID 0)
-- Dependencies: 188
-- Name: weights_relationid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('weights_relationid_seq', 1, false);


--
-- TOC entry 2164 (class 0 OID 0)
-- Dependencies: 187
-- Name: weights_userid_seq; Type: SEQUENCE SET; Schema: public; Owner: mushu
--

SELECT pg_catalog.setval('weights_userid_seq', 1, false);


--
-- TOC entry 2032 (class 2606 OID 17410)
-- Dependencies: 168 168 2104
-- Name: articles_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_primarykey PRIMARY KEY (articleid);


--
-- TOC entry 2034 (class 2606 OID 17421)
-- Dependencies: 171 171 2104
-- Name: collections_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections
    ADD CONSTRAINT collections_primarykey PRIMARY KEY (collectionid);


--
-- TOC entry 2030 (class 2606 OID 17396)
-- Dependencies: 165 165 2104
-- Name: descriptions_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions
    ADD CONSTRAINT descriptions_primarykey PRIMARY KEY (descriptionid);


--
-- TOC entry 2036 (class 2606 OID 17441)
-- Dependencies: 177 177 2104
-- Name: discussions_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions
    ADD CONSTRAINT discussions_primarykey PRIMARY KEY (discussionid);


--
-- TOC entry 2038 (class 2606 OID 17467)
-- Dependencies: 185 185 2104
-- Name: relations_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_primarykey PRIMARY KEY (relationid);


--
-- TOC entry 2040 (class 2606 OID 17488)
-- Dependencies: 191 191 2104
-- Name: results_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY results
    ADD CONSTRAINT results_primarykey PRIMARY KEY (resultid);


--
-- TOC entry 2026 (class 2606 OID 17379)
-- Dependencies: 162 162 2104
-- Name: users_primarykey; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_primarykey PRIMARY KEY (userid);


--
-- TOC entry 2028 (class 2606 OID 17381)
-- Dependencies: 162 162 2104
-- Name: users_username_unique; Type: CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_username_unique UNIQUE (username);


--
-- TOC entry 2043 (class 2606 OID 17613)
-- Dependencies: 168 165 2029 2104
-- Name: articles_descriptionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY articles
    ADD CONSTRAINT articles_descriptionid FOREIGN KEY (descriptionid) REFERENCES descriptions(descriptionid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2061 (class 2606 OID 17518)
-- Dependencies: 168 2031 200 2104
-- Name: children_child; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children
    ADD CONSTRAINT children_child FOREIGN KEY (child) REFERENCES articles(articleid) MATCH FULL;


--
-- TOC entry 2062 (class 2606 OID 17523)
-- Dependencies: 200 168 2031 2104
-- Name: children_parent; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY children
    ADD CONSTRAINT children_parent FOREIGN KEY (parent) REFERENCES articles(articleid) MATCH FULL;


--
-- TOC entry 2057 (class 2606 OID 17538)
-- Dependencies: 194 2033 171 2104
-- Name: choices_collectionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices
    ADD CONSTRAINT choices_collectionid FOREIGN KEY (collectionid) REFERENCES collections(collectionid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2058 (class 2606 OID 17543)
-- Dependencies: 2039 194 191 2104
-- Name: choices_resultid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY choices
    ADD CONSTRAINT choices_resultid FOREIGN KEY (resultid) REFERENCES results(resultid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2045 (class 2606 OID 17598)
-- Dependencies: 2031 168 174 2104
-- Name: collectedarticles_articleid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles
    ADD CONSTRAINT collectedarticles_articleid FOREIGN KEY (articleid) REFERENCES articles(articleid) MATCH FULL;


--
-- TOC entry 2046 (class 2606 OID 17603)
-- Dependencies: 2033 171 174 2104
-- Name: collectedarticles_collectionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collectedarticles
    ADD CONSTRAINT collectedarticles_collectionid FOREIGN KEY (collectionid) REFERENCES collections(collectionid) MATCH FULL;


--
-- TOC entry 2044 (class 2606 OID 17608)
-- Dependencies: 165 2029 171 2104
-- Name: collections_descriptionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY collections
    ADD CONSTRAINT collections_descriptionid FOREIGN KEY (descriptionid) REFERENCES descriptions(descriptionid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2042 (class 2606 OID 17618)
-- Dependencies: 2025 165 162 2104
-- Name: descriptions_author; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY descriptions
    ADD CONSTRAINT descriptions_author FOREIGN KEY (author) REFERENCES users(userid) MATCH FULL;


--
-- TOC entry 2048 (class 2606 OID 17593)
-- Dependencies: 177 2033 171 2104
-- Name: discussions_collectionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions
    ADD CONSTRAINT discussions_collectionid FOREIGN KEY (collectionid) REFERENCES collections(collectionid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2047 (class 2606 OID 17588)
-- Dependencies: 191 2039 177 2104
-- Name: discussions_result; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY discussions
    ADD CONSTRAINT discussions_result FOREIGN KEY (resultid) REFERENCES results(resultid) MATCH FULL;


--
-- TOC entry 2050 (class 2606 OID 17583)
-- Dependencies: 180 177 2035 2104
-- Name: participants_discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants
    ADD CONSTRAINT participants_discussionid FOREIGN KEY (discussionid) REFERENCES discussions(discussionid) MATCH FULL;


--
-- TOC entry 2049 (class 2606 OID 17578)
-- Dependencies: 2025 162 180 2104
-- Name: participants_userid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY participants
    ADD CONSTRAINT participants_userid FOREIGN KEY (userid) REFERENCES users(userid) MATCH FULL;


--
-- TOC entry 2051 (class 2606 OID 17563)
-- Dependencies: 185 2029 165 2104
-- Name: relations_description; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_description FOREIGN KEY ("descriptionId") REFERENCES descriptions(descriptionid) MATCH FULL;


--
-- TOC entry 2053 (class 2606 OID 17573)
-- Dependencies: 185 168 2031 2104
-- Name: relations_source; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_source FOREIGN KEY (source) REFERENCES articles(articleid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2052 (class 2606 OID 17568)
-- Dependencies: 2031 185 168 2104
-- Name: relations_target; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY relations
    ADD CONSTRAINT relations_target FOREIGN KEY (target) REFERENCES articles(articleid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2041 (class 2606 OID 17623)
-- Dependencies: 2031 162 168 2104
-- Name: users_profile; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_profile FOREIGN KEY (profile) REFERENCES articles(articleid) MATCH FULL;


--
-- TOC entry 2060 (class 2606 OID 17533)
-- Dependencies: 191 197 2039 2104
-- Name: voters_resultid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters
    ADD CONSTRAINT voters_resultid FOREIGN KEY (resultid) REFERENCES results(resultid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2059 (class 2606 OID 17528)
-- Dependencies: 197 2025 162 2104
-- Name: voters_userid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY voters
    ADD CONSTRAINT voters_userid FOREIGN KEY (userid) REFERENCES users(userid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2056 (class 2606 OID 17558)
-- Dependencies: 177 2035 189 2104
-- Name: weights_discussionid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY weights
    ADD CONSTRAINT weights_discussionid FOREIGN KEY (discussionid) REFERENCES discussions(discussionid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2054 (class 2606 OID 17548)
-- Dependencies: 185 2037 189 2104
-- Name: weights_relationid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY weights
    ADD CONSTRAINT weights_relationid FOREIGN KEY (relationid) REFERENCES relations(relationid) MATCH FULL ON DELETE CASCADE;


--
-- TOC entry 2055 (class 2606 OID 17553)
-- Dependencies: 189 2025 162 2104
-- Name: weights_userid; Type: FK CONSTRAINT; Schema: public; Owner: mushu
--

ALTER TABLE ONLY weights
    ADD CONSTRAINT weights_userid FOREIGN KEY (userid) REFERENCES users(userid) MATCH FULL ON DELETE CASCADE;


-- Completed on 2013-05-03 01:18:56 CEST

--
-- PostgreSQL database dump complete
--

