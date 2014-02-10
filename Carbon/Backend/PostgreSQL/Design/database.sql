-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- PostgreSQL version: 9.3
-- Project Site: pgmodeler.com.br
-- Model Author: Jakob Runge

SET check_function_bodies = false;
-- ddl-end --


-- Database creation must be done outside an multicommand file.
-- These commands were put in this file only for convenience.
-- -- object: openbrain | type: DATABASE --
-- CREATE DATABASE openbrain
-- 	ENCODING = 'UTF8'
-- ;
-- -- ddl-end --
-- 

-- object: public.users | type: TABLE --
CREATE TABLE public.users(
	userid serial NOT NULL,
	username varchar(255) NOT NULL,
	hash varchar(128) NOT NULL,
	salt varchar(255) NOT NULL,
	creationtime timestamp NOT NULL DEFAULT now(),
	lastlogin timestamp NOT NULL DEFAULT now(),
	isadmin boolean NOT NULL DEFAULT false,
	profile integer DEFAULT NULL,
	sessionkey varchar(255),
	CONSTRAINT users_primarykey PRIMARY KEY (userid),
	CONSTRAINT users_username_unique UNIQUE (username)

)WITH ( OIDS = TRUE );
-- ddl-end --
-- object: public.descriptions | type: TABLE --
CREATE TABLE public.descriptions(
	descriptionid serial NOT NULL,
	headline varchar(255) NOT NULL,
	summary varchar(255) NOT NULL,
	CONSTRAINT description_descriptionid PRIMARY KEY (descriptionid)

);
-- ddl-end --
COMMENT ON TABLE public.descriptions IS 'Table for OpenBrain.Data.Description';
-- ddl-end --
-- ddl-end --

-- object: public.articles | type: TABLE --
CREATE TABLE public.articles(
	articleid serial NOT NULL,
	content text NOT NULL,
	CONSTRAINT articles_articleid PRIMARY KEY (articleid)

);
-- ddl-end --
COMMENT ON TABLE public.articles IS 'Table for OpenBrain.Data.Article';
-- ddl-end --
-- ddl-end --

-- object: public.acceptanceconditions | type: TABLE --
CREATE TABLE public.acceptanceconditions(
	acceptanceconditionid serial NOT NULL,
	proofstandard smallint,
	formula text NOT NULL,
	CONSTRAINT acceptanceconditions_acceptanceconditionid PRIMARY KEY (acceptanceconditionid)

);
-- ddl-end --
COMMENT ON TABLE public.acceptanceconditions IS 'Table for OpenBrain.Data.AcceptanceCondition';
-- ddl-end --
-- ddl-end --

-- object: public.relations | type: TABLE --
CREATE TABLE public.relations(
	relationid serial NOT NULL,
	source integer NOT NULL,
	target integer NOT NULL,
	relationtype smallint NOT NULL,
	CONSTRAINT relation_relationid PRIMARY KEY (relationid)

);
-- ddl-end --
COMMENT ON TABLE public.relations IS 'Table for OpenBrain.Data.Relation';
-- ddl-end --
-- ddl-end --

-- object: public.discussions | type: TABLE --
CREATE TABLE public.discussions(
	discussionid serial NOT NULL,
	deadline timestamp DEFAULT NULL,
	evaluation smallint NOT NULL,
	CONSTRAINT discussion_discussionid PRIMARY KEY (discussionid)

);
-- ddl-end --
COMMENT ON TABLE public.discussions IS 'Table for OpenBrain.Data.Discussion';
-- ddl-end --
-- ddl-end --

-- object: public.discussion_arguments | type: TABLE --
CREATE TABLE public.discussion_arguments(
	discussionid integer NOT NULL,
	itemid integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public.discussion_arguments IS 'Field arguments for OpenBrain.Data.Discussion';
-- ddl-end --
-- ddl-end --

-- object: public.discussion_participants | type: TABLE --
CREATE TABLE public.discussion_participants(
	discussionid integer NOT NULL,
	userid integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public.discussion_participants IS 'participants field for OpenBrain.Data.Discussion';
-- ddl-end --
-- ddl-end --

-- object: public.resultsets | type: TABLE --
CREATE TABLE public.resultsets(
	resultsetid serial NOT NULL,
	setcreation timestamp NOT NULL DEFAULT now(),
	CONSTRAINT resultset_resultsetid PRIMARY KEY (resultsetid)

);
-- ddl-end --
COMMENT ON TABLE public.resultsets IS 'Table for OpenBrain.Data.ResultSet';
-- ddl-end --
-- ddl-end --

-- object: public.results | type: TABLE --
CREATE TABLE public.results(
	resultid serial NOT NULL,
	resulttype varchar(255) NOT NULL DEFAULT '[]',
	votes integer NOT NULL DEFAULT 0,
	CONSTRAINT result_resultid PRIMARY KEY (resultid)

);
-- ddl-end --
COMMENT ON TABLE public.results IS 'Table for OpenBrain.Data.Result';
-- ddl-end --
-- ddl-end --

-- object: public.results_items | type: TABLE --
CREATE TABLE public.results_items(
	resultid integer NOT NULL,
	resultstate smallint NOT NULL,
	itemid integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public.results_items IS 'Field items of OpenBrain.Data.Result';
-- ddl-end --
-- ddl-end --

-- object: public.resultset_results | type: TABLE --
CREATE TABLE public.resultset_results(
	resultsetid integer NOT NULL,
	resultid integer NOT NULL
);
-- ddl-end --
-- object: public.resultset_voters | type: TABLE --
CREATE TABLE public.resultset_voters(
	resultsetid integer NOT NULL,
	userid integer NOT NULL,
	voted bool NOT NULL DEFAULT false
);
-- ddl-end --
-- object: public.items | type: TABLE --
CREATE TABLE public.items(
	itemid serial NOT NULL,
	descriptionid integer DEFAULT NULL,
	articleid integer DEFAULT NULL,
	acceptanceconditionid integer DEFAULT NULL,
	relationid integer DEFAULT NULL,
	discussionid integer DEFAULT NULL,
	resultsetid integer DEFAULT NULL,
	creation timestamp NOT NULL DEFAULT now(),
	deletion timestamp DEFAULT NULL,
	commitmessage varchar(255) NOT NULL,
	commitauthor integer NOT NULL,
	CONSTRAINT items_itemid PRIMARY KEY (itemid)

);
-- ddl-end --
COMMENT ON TABLE public.items IS 'Table for OpenBrain.Data.Item';
-- ddl-end --
-- ddl-end --

-- object: public.item_family | type: TABLE --
CREATE TABLE public.item_family(
	parent integer NOT NULL,
	child integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public.item_family IS 'Fields parents and children of OpenBrain.Data.Item';
-- ddl-end --
-- ddl-end --

-- object: relation_source | type: CONSTRAINT --
ALTER TABLE public.relations ADD CONSTRAINT relation_source FOREIGN KEY (source)
REFERENCES public.items (itemid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: relation_target | type: CONSTRAINT --
ALTER TABLE public.relations ADD CONSTRAINT relation_target FOREIGN KEY (target)
REFERENCES public.items (itemid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: discussion_arguments_discussion | type: CONSTRAINT --
ALTER TABLE public.discussion_arguments ADD CONSTRAINT discussion_arguments_discussion FOREIGN KEY (discussionid)
REFERENCES public.discussions (discussionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: discussion_arguments_item | type: CONSTRAINT --
ALTER TABLE public.discussion_arguments ADD CONSTRAINT discussion_arguments_item FOREIGN KEY (itemid)
REFERENCES public.items (itemid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: discussion_participants_discussion | type: CONSTRAINT --
ALTER TABLE public.discussion_participants ADD CONSTRAINT discussion_participants_discussion FOREIGN KEY (discussionid)
REFERENCES public.discussions (discussionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: discussion_participants_participant | type: CONSTRAINT --
ALTER TABLE public.discussion_participants ADD CONSTRAINT discussion_participants_participant FOREIGN KEY (userid)
REFERENCES public.users (userid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: results_items_result | type: CONSTRAINT --
ALTER TABLE public.results_items ADD CONSTRAINT results_items_result FOREIGN KEY (resultid)
REFERENCES public.results (resultid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: results_items_item | type: CONSTRAINT --
ALTER TABLE public.results_items ADD CONSTRAINT results_items_item FOREIGN KEY (itemid)
REFERENCES public.items (itemid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: resuletset_results_resultset | type: CONSTRAINT --
ALTER TABLE public.resultset_results ADD CONSTRAINT resuletset_results_resultset FOREIGN KEY (resultsetid)
REFERENCES public.resultsets (resultsetid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: resultset_results_result | type: CONSTRAINT --
ALTER TABLE public.resultset_results ADD CONSTRAINT resultset_results_result FOREIGN KEY (resultid)
REFERENCES public.results (resultid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: resultset_voters_resultset | type: CONSTRAINT --
ALTER TABLE public.resultset_voters ADD CONSTRAINT resultset_voters_resultset FOREIGN KEY (resultsetid)
REFERENCES public.resultsets (resultsetid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: resultset_voters_voter | type: CONSTRAINT --
ALTER TABLE public.resultset_voters ADD CONSTRAINT resultset_voters_voter FOREIGN KEY (userid)
REFERENCES public.users (userid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: items_commitauthor | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT items_commitauthor FOREIGN KEY (commitauthor)
REFERENCES public.users (userid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: items_descriptionid | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT items_descriptionid FOREIGN KEY (descriptionid)
REFERENCES public.descriptions (descriptionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: items_articleid | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT items_articleid FOREIGN KEY (articleid)
REFERENCES public.articles (articleid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: items_acceptancecondition | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT items_acceptancecondition FOREIGN KEY (acceptanceconditionid)
REFERENCES public.acceptanceconditions (acceptanceconditionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: items_relation | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT items_relation FOREIGN KEY (relationid)
REFERENCES public.relations (relationid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: item_discussion | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT item_discussion FOREIGN KEY (discussionid)
REFERENCES public.discussions (discussionid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: item_resultset | type: CONSTRAINT --
ALTER TABLE public.items ADD CONSTRAINT item_resultset FOREIGN KEY (resultsetid)
REFERENCES public.resultsets (resultsetid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: item_parent | type: CONSTRAINT --
ALTER TABLE public.item_family ADD CONSTRAINT item_parent FOREIGN KEY (parent)
REFERENCES public.items (itemid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --


-- object: item_child | type: CONSTRAINT --
ALTER TABLE public.item_family ADD CONSTRAINT item_child FOREIGN KEY (child)
REFERENCES public.items (itemid) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION NOT DEFERRABLE;
-- ddl-end --



