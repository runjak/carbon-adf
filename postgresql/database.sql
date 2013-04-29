/* Database generated with pgModeler (PostgreSQL Database Modeler).
  Project Site: pgmodeler.com.br
  Model Author: Jakob Runge */

SET check_function_bodies = false;
-- ddl-end --


/* Database creation must be done outside an multicommand file.
   These commands were put in this file only for convenience.

-- object: openbrain | type: DATABASE -- 
CREATE DATABASE openbrain
	ENCODING = 'UTF8'
;
-- ddl-end --


*/

-- object: public.users | type: TABLE -- 
CREATE TABLE public.users(
	userid serial NOT NULL,
	username varchar(255) NOT NULL,
	hash varchar(32) NOT NULL,
	salt varchar(255) NOT NULL,
	creationtime timestamp NOT NULL DEFAULT now(),
	lastlogin timestamp NOT NULL DEFAULT now(),
	isadmin boolean NOT NULL DEFAULT false,
	profile integer DEFAULT NULL,
	sessionkey varchar(255),
	CONSTRAINT users_primarykey PRIMARY KEY (userid),
	CONSTRAINT users_username_unique UNIQUE (username)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.descriptions | type: TABLE -- 
CREATE TABLE public.descriptions(
	descriptionid serial NOT NULL,
	author serial NOT NULL,
	headline varchar(255) NOT NULL,
	description text NOT NULL,
	creation timestamp NOT NULL DEFAULT now(),
	deletion timestamp DEFAULT NULL,
	CONSTRAINT descriptions_primarykey PRIMARY KEY (descriptionid)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.articles | type: TABLE -- 
CREATE TABLE public.articles(
	articleid serial NOT NULL,
	descriptionid serial NOT NULL,
	content text NOT NULL,
	CONSTRAINT articles_primarykey PRIMARY KEY (articleid)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.collections | type: TABLE -- 
CREATE TABLE public.collections(
	collectionid serial NOT NULL,
	descriptionid serial NOT NULL,
	CONSTRAINT collections_primarykey PRIMARY KEY (collectionid)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.collectedarticles | type: TABLE -- 
CREATE TABLE public.collectedarticles(
	collectionid serial NOT NULL,
	articleid serial NOT NULL
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.discussions | type: TABLE -- 
CREATE TABLE public.discussions(
	discussionid serial NOT NULL,
	collectionid serial NOT NULL,
	deadline timestamp NOT NULL,
	resultid integer DEFAULT NULL,
	CONSTRAINT discussions_primarykey PRIMARY KEY (discussionid)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.participants | type: TABLE -- 
CREATE TABLE public.participants(
	discussionid serial NOT NULL,
	userid serial NOT NULL
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.relations | type: TABLE -- 
CREATE TABLE public.relations(
	relationid serial NOT NULL,
	source serial NOT NULL,
	target serial NOT NULL,
	type varchar(255) NOT NULL,
	CONSTRAINT relations_primarykey PRIMARY KEY (relationid)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.weights | type: TABLE -- 
CREATE TABLE public.weights(
	discussionid serial NOT NULL,
	userid serial NOT NULL,
	relationid serial NOT NULL,
	weight integer NOT NULL DEFAULT 0
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.results | type: TABLE -- 
CREATE TABLE public.results(
	resultid serial NOT NULL,
	CONSTRAINT results_primarykey PRIMARY KEY (resultid)
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.choices | type: TABLE -- 
CREATE TABLE public.choices(
	resultid serial NOT NULL,
	collectionid serial NOT NULL,
	votes integer NOT NULL DEFAULT 0
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.voters | type: TABLE -- 
CREATE TABLE public.voters(
	resultid serial NOT NULL,
	userid serial NOT NULL,
	voted boolean NOT NULL DEFAULT false
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: public.children | type: TABLE -- 
CREATE TABLE public.children(
	parent serial NOT NULL,
	child serial NOT NULL
)
WITH (OIDS=TRUE);
-- ddl-end --

-- ddl-end --

-- object: children_child | type: CONSTRAINT -- 
ALTER TABLE public.children ADD CONSTRAINT children_child FOREIGN KEY (child)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: children_parent | type: CONSTRAINT -- 
ALTER TABLE public.children ADD CONSTRAINT children_parent FOREIGN KEY (parent)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: voters_userid | type: CONSTRAINT -- 
ALTER TABLE public.voters ADD CONSTRAINT voters_userid FOREIGN KEY (userid)
REFERENCES public.users (userid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: voters_resultid | type: CONSTRAINT -- 
ALTER TABLE public.voters ADD CONSTRAINT voters_resultid FOREIGN KEY (resultid)
REFERENCES public.results (resultid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: choices_collectionid | type: CONSTRAINT -- 
ALTER TABLE public.choices ADD CONSTRAINT choices_collectionid FOREIGN KEY (collectionid)
REFERENCES public.collections (collectionid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: choices_resultid | type: CONSTRAINT -- 
ALTER TABLE public.choices ADD CONSTRAINT choices_resultid FOREIGN KEY (resultid)
REFERENCES public.results (resultid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: weights_relationid | type: CONSTRAINT -- 
ALTER TABLE public.weights ADD CONSTRAINT weights_relationid FOREIGN KEY (relationid)
REFERENCES public.relations (relationid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: weights_userid | type: CONSTRAINT -- 
ALTER TABLE public.weights ADD CONSTRAINT weights_userid FOREIGN KEY (userid)
REFERENCES public.users (userid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: weights_discussionid | type: CONSTRAINT -- 
ALTER TABLE public.weights ADD CONSTRAINT weights_discussionid FOREIGN KEY (discussionid)
REFERENCES public.discussions (discussionid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: relations_target | type: CONSTRAINT -- 
ALTER TABLE public.relations ADD CONSTRAINT relations_target FOREIGN KEY (target)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: relations_source | type: CONSTRAINT -- 
ALTER TABLE public.relations ADD CONSTRAINT relations_source FOREIGN KEY (source)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: participants_userid | type: CONSTRAINT -- 
ALTER TABLE public.participants ADD CONSTRAINT participants_userid FOREIGN KEY (userid)
REFERENCES public.users (userid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: participants_discussionid | type: CONSTRAINT -- 
ALTER TABLE public.participants ADD CONSTRAINT participants_discussionid FOREIGN KEY (discussionid)
REFERENCES public.discussions (discussionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: discussions_result | type: CONSTRAINT -- 
ALTER TABLE public.discussions ADD CONSTRAINT discussions_result FOREIGN KEY (resultid)
REFERENCES public.results (resultid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: discussions_collectionid | type: CONSTRAINT -- 
ALTER TABLE public.discussions ADD CONSTRAINT discussions_collectionid FOREIGN KEY (collectionid)
REFERENCES public.collections (collectionid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: collectedarticles_articleid | type: CONSTRAINT -- 
ALTER TABLE public.collectedarticles ADD CONSTRAINT collectedarticles_articleid FOREIGN KEY (articleid)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: collectedarticles_collectionid | type: CONSTRAINT -- 
ALTER TABLE public.collectedarticles ADD CONSTRAINT collectedarticles_collectionid FOREIGN KEY (collectionid)
REFERENCES public.collections (collectionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: collections_descriptionid | type: CONSTRAINT -- 
ALTER TABLE public.collections ADD CONSTRAINT collections_descriptionid FOREIGN KEY (descriptionid)
REFERENCES public.descriptions (descriptionid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: articles_descriptionid | type: CONSTRAINT -- 
ALTER TABLE public.articles ADD CONSTRAINT articles_descriptionid FOREIGN KEY (descriptionid)
REFERENCES public.descriptions (descriptionid) MATCH FULL
ON DELETE CASCADE ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: descriptions_author | type: CONSTRAINT -- 
ALTER TABLE public.descriptions ADD CONSTRAINT descriptions_author FOREIGN KEY (author)
REFERENCES public.users (userid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: users_profile | type: CONSTRAINT -- 
ALTER TABLE public.users ADD CONSTRAINT users_profile FOREIGN KEY (profile)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --



