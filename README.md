# What is it
Lola is news application.

# How does it work
This application is designed to post and read the news. Read news can everybody. Users need to register to post and comment news. 

News has categories and tags.

## How to make a post

To post news user must be an author. Only admin can make a user an author. 

At first author should create draft and then publish it.

To update post author should create draft of this post and publish it.

# Installing
You can install app with:

    $ git clone https://github.com/JekaGrib/lola.git

And then in app directory:

    $ stack build


# Getting started
## 1. Create database
Application use PostgreSQL as database.

Before start using app you should create new database and enter details in [Configuration](#2-configuration)

There is database diagram
![](https://github.com/JekaGrib/lola/raw/master/other/pic/dbDiagram.png)

To create database structure you can use file "dbStructure.sql"

## 2. Add default entities to db
Application has several default db entities:
1. default picture  - picture of deleted user
2. default user     - deleted user
2. default author   - deleted author
2. default category - deleted category

You can create yours default entities or use folder "migrations" with migrations examples.

For all example migrations you can use file "COMMAND for all migrations with dbStructure" for psql.

Aftеr creating entities enter ids in [Configuration](#2-configuration)

## 3. Configuration
Before start, you should rename "example.config" to "postApp.config". 
Then you should make changes in this file.

There is table with descriptions of each values in configuration file, that should be replace to your values.
|Section      | Value                | Description                       | 
|:------------| :------------------- |:------------------------------- |
|Server       | host                 | Server host     |
|Server       | port                 | Server port           |
|Database     | host                 | Database host  |
|Database     | port                 | Database port               |
|Database     | dbname               | Database name               |
|Database     | password             | Database password           |
|defaultValues| defaultPictureId     | Id picture of deleted user  |
|defaultValues| defaultUserId        | Id of deleted user          |
|defaultValues| defaultAuthorId      | Id of deleted author        |
|defaultValues| defaultCategoryId    | Id of deleted category      |
|LimitNumbers | commentNumberLimit   | The number of comments given in the response at a time |
|LimitNumbers | draftNumberLimit     | The number of drafts given in the response at a time |
|LimitNumbers | postNumberLimit      | The number of posts given in the response at a time |
|log          | logLevel             | The logging level is specified here. The log will only display entries of this level and levels above. It can be one of four levels: DEBUG,INFO,WARNING,ERROR. More information [here](#logging)  |

## 4. Run
You can run App with:

    $ stack exec lola-exe 

# Api

INT - number from 0 to 9223372036854775805 for id and from 0 to 100000 for page

Success answers:
1. For endpoint GET :
    *  Status 200 OK with resourse entity in request body.
2. For endpoint POST :
    *  Status 201 Created with Location of created entity in Headers.
3. For endpoint PUT (Put only update resourse, NOT create new ) :
    *  Status 200 OK with entity of resourse in request body.
4. For endpoint DELETE :
    *  Status 204 No Data

Fail answers:
* Status 400 BadRequest with JSON body {«ok»:«false»,«info»:«Some error info»}
* Status 401 Unauthorized with JSON body {«ok»:«false»,«info»:«Some error info»}
* Status 404 (resourse or resourse entity doesn`t exist)
* Status 413 Request Body Too Large
* Status 414 Request-URI Too Long
* Status 500 Internal server error
* Status 501 Not implemented with JSON body {«ok»:«false»,«info»:«Some error info»}


Methods:
1. User methods :
    1. To create user:  
    POST /users  
    Query parameters:  
    - password TXT (max50char)
    - first_name TXT (max50char)
    - last_name TXT (max50char)
    - user_pic_id INT
    
    Json answer example:

        {"token":"abc"}

    2. To create admin:  
    POST /admins  
    Query parameters:  
    - create_admin_key TXT (max50char)
    - password TXT (max50char)
    - first_name TXT (max50char)
    - last_name TXT (max50char)
    - user_pic_id INT
    
    Json answer example:

        {"token":"abc"}

    3. To get user:  
    GET /users/INT(user_id)  
    
    Json answer example:

        {"user_id":4,"first_name":"Petronella","last_name":"Gillingham","user_pic_id":235,"user_pic_url":"http://localhost:3000/pictures/235","user_create_date":"2021-08-01"}

    4. To delete user:  
    DELETE /users/user_id INT  
    Query parameters:
    - token TXT (admin token)
           
    5. To log in:  
    POST /logIn  
    Query parameters:
    - user_id INT
    - password TXT (max50char)
    
    Json answer example:

        {"token":"abc"}

2. Author methods:
    1. To create author:  
    POST /authors  
    Query parameters:
    - user_id INT
    - author_info TXT (max500char)
    - token TXT (admin token)
           
    2. To get author:  
    GET /authors/INT(author_id)  
    Query parameters:
    - token TXT (admin token)

    Json answer example:  
        
        {"author_id":153,"author_info":"dimonnnn","user_id":400}

    3. To update author:  
    PUT /authors/INT(author_id)  
    Query parameters:
    - user_id INT
    - author_info TXT (max500char)
    - token TXT (admin token)

    Json answer example:
        
        {"author_id":153,"author_info":"dimonnnn","user_id":400}

    4. To delete author:  
    DELETE /authors/INT(author_id)  
    Query parameters:
    - token TXT (admin token)
           
3. Category methods :
    1. To create category:  
    POST /categories  
    Query parameters:
    - category_name TXT (max50char)
    - super_category_id INT optional
    - token TXT (admin token)
           
    2. To get category:  
    GET /categories/INT(category_id)  

    Json answer example:

        {"category_id":53,"category_name":"primis","sub_categories":[61],"super_category":{"category_id":7,"category_name":"ut","sub_categories":[52,53]}}

    3. To update category:  
    PUT /categories/category_id INT  
    Query parameters:
    - category_name TXT (max50char)
    - super_category_id INT  optional
    - token TXT (admin token)

    Json answer example:
        
        {"category_id":201,"category_name":"dim","sub_categories":[11,17]}

    4. To delete category:  
    DELETE /categories/category_id INT  
    Query parameters:  
    - token TXT (admin token)
           
4. Tag methods :
    1. To create tag:  
    POST /tags  
    Query parameters:  
    - tag_name TXT (max50char)
    - token TXT (admin token)
           
    2. To get tag:  
    GET /tags/INT(tag_id)  

    Json answer example:
        
        {"tag_id":7,"tag_name":"facilisi"}

    3. To update tag:  
    PUT /tags/tag_id INT  
    Query parameters:
    - tag_name TXT (max50char)
    - token TXT (admin token) 

    Json answer example:

        {"tag_id":151,"tag_name":"snow"}

    4. To delete tag:
    DELETE /tags/tag_id INT  
    Query parameters:
    - token TXT (admin token)
           
5. Pictures methods :
    1. To load  picture from url:  
    POST /pictures  
    Query parameters:  
    - pic_url TXT

    2. To get picture:  
    GET /pictures/INT(pic_id)  
    
    Answer will be picture

6. Draft methods:  
    1. To create new draft:  
    POST /drafts  
    User should be author.  
    Query parameters:  
    - token TXT (user/admin token)
    JSON parameters:
    - draft_name TXT (max50char)
    - draft_category_id INT
    - draft_text TXT (max10000char)
    - draft_main_pic_id INT
    - draft_tags_ids INT ARRAY 
    - draft_pics_ids  INT ARRAY

    Request example:
        
        { "token": "abc",
        "draft_name": "rock",
        "draft_category_id": 3,
        "draft_text": "heyhey",
        "draft_main_pic_id": 501,
        "draft_tags_ids" : [ 1, 2, 4 ],
        "draft_pics_ids": [ 5,4,3]
        }
           
    2. To create draft for post:  
    POST posts/post_id INT/drafts  
    User should be post author.  
    Query parameters:
    - token TXT (user/admin token)
           
    3. To publish draft (create post(if it is new draft) or update post(if it is post`s draft)):  
    POST drafts/draft_id INT/posts  
    User should be draft author.  
    Query parameters:
    - token TXT (user/admin token)
           
    Json answer example (for update post):

        {"post_id":7,"author":{"author_id":2,"author_info":"info","user_id":2},"post_name":"name","post_create_date":"2018-06-26","post_category":{"category_id":16,"category_name":"odio","sub_categories":[25,26]},"post_text":"text","post_main_pic_id":164,"post_main_pic_url":"http://localhost:3000/pictures/164","post_pics":[{"pic_id":338,"pic_url":"http://localhost:3000/pictures/338"},{"pic_id":356,"pic_url":"http://localhost:3000/pictures/356"}],"post_tags":[{"tag_id":103,"tag_name":"consequat"},{"tag_id":118,"tag_name":"cum"}]}

    4. To get draft:  
    GET /drafts/draft_id INT  
    User should be draft author.  
    Query parameters:
    - token TXT (user/admin token)

    Json answer example:

        {"draft_id":155,"post_id":20,"author":{"author_id":27,"author_info":"info","user_id":27},"draft_name":"aname","draft_category":{"category_id":50,"category_name":"enim","sub_categories":[65,93],"super_category":{"category_id":3,"category_name":"quisque","sub_categories":[33,50,60]}}},"draft_text":"itext","draft_main_pic_id":462,"draft_main_pic_url":"http://localhost:3000/pictures/462","draft_pics":[{"pic_id":279,"pic_url":"http://localhost:3000/pictures/279"},{"pic_id":133,"pic_url":"http://localhost:3000/pictures/133"}],"draft_tags":[{"tag_id":98,"tag_name":"ut"},{"tag_id":71,"tag_name":"quisque"}]}

    5. To get several drafts for author:  
    GET /drafts  
    User should be author, gets only his drafts.  
    Query parameters:  
    - page INT
    - token TXT (user/admin token)

    Json answer example:
        
        {"page":2,"drafts":[{"draft_id":136,"post_id":"NULL","author":{"author_id":27,"author_info":"info","user_id":27},"draft_name":"cname","draft_category":{"category_id":17,"category_name":"varius","sub_categories":[42,51]}}}},"draft_text":"lorem quisque","draft_main_pic_id":66,"draft_main_pic_url":"http://localhost:3000/pictures/66","draft_pics":[{"pic_id":152,"pic_url":"http://localhost:3000/pictures/152"},{"pic_id":245,"pic_url":"http://localhost:3000/pictures/245"}],"draft_tags":[{"tag_id":13,"tag_name":"faucibus"},{"tag_id":50,"tag_name":"vitae"}]},{"draft_id":52,"post_id":"NULL","author":{"author_id":27,"author_info":"info","user_id":27},"draft_name":"id lobortis","draft_category":{"category_id":29,"category_name":"sollicitudin","sub_categories":[62],"super_category":{"category_id":11,"category_name":"id","sub_categories":[27,29,36,38,47]}}}}},"draft_text":"text","draft_main_pic_id":202,"draft_main_pic_url":"http://localhost:3000/pictures/202","draft_pics":[{"pic_id":500,"pic_url":"http://localhost:3000/pictures/500"}],"draft_tags":[{"tag_id":20,"tag_name":"mauris"},{"tag_id":100,"tag_name":"ac"}]}

    6. To update draft:  
    PUT drafts/INT(draft_id).  
    User should be draft author.  
    Query parameters:  
    - token TXT (user/admin token)  
    JSON parameters:
    - draft_name TXT (max50char)
    - draft_category_id INT
    - draft_text TXT (max10000char)
    - draft_main_pic_id INT
    - draft_tags_ids INT ARRAY
    - draft_pics_ids INT ARRAY

    Json answer example:
        
        {"draft_id":152,"post_id":"NULL","author":{"author_id":27,"author_info":"info,"user_id":27},"draft_name":"rock","draft_category":{"category_id":3,"category_name":"quisque","sub_categories":[33,50,60]},"draft_text":"text","draft_main_pic_id":420,"draft_main_pic_url":"http://localhost:3000/pictures/420","draft_pics":[{"pic_id":5,"pic_url":"http://localhost:3000/pictures/5"},{"pic_id":42,"pic_url":"http://localhost:3000/pictures/42"}],"draft_tags":[{"tag_id":1,"tag_name":"interdum"},{"tag_id":27,"tag_name":"eu"}]}

    7. To delete draft:  
    DELETE /drafts/draft_id INT  
    User should be draft author.  
    Query parameters:
    - token TXT (user/admin token)
           
7. Post methods:  
    1. To get one post:  
    GET /posts/INT(post_id)  
    
    Json answer example:
        
        {"post_id":7,"author":{"author_id":2,"author_info":"info","user_id":2},"post_name":"name","post_create_date":"2018-06-26","post_category":{"category_id":16,"category_name":"odio","sub_categories":[25,26]},"post_text":"text","post_main_pic_id":164,"post_main_pic_url":"http://localhost:3000/pictures/164","post_pics":[{"pic_id":338,"pic_url":"http://localhost:3000/pictures/338"},{"pic_id":356,"pic_url":"http://localhost:3000/pictures/356"}],"post_tags":[{"tag_id":103,"tag_name":"consequat"},{"tag_id":118,"tag_name":"cum"}]}

    2. To get several posts:  
    GET /posts  
    Query parameters required :  
    - page INT  
    Query parameters optional:  
    - Filter Query parameters:
        + Only one of three date filters:
            * created_at DATE (Format yyyy-mm-dd, example 2020-02-02)
            * created_at_gt DATE (Format yyyy-mm-dd, example 2020-02-02)
            * created_at_lt DATE (Format yyyy-mm-dd, example 2020-02-02)
        + Only one of three tag filters:
            * tag INT
            * tags_in ARRAY of INT 
            * tags_all ARRAY of INT 
        + Only one of three include filters:
            * name_in TXT
            * text_in TXT
            * everywhere_in TXT
              (everywhere search in post text, post name, authors first name, category name, tag name)
        + category_id INT 
        + author_name TXT 
    - Sort Query parameters:  
    Default sort — DESC by date,DECS by post_id. Sort value can be only «asc» or «desc». If there are more then one sort Query parameters, sort priority corresponds to the order in which the parameter appears in the query string. Two last extra sort layers is always by date and post id (DESC by default or ASC — if there is «sort_by_date=asc» in query string in any place)
        + sort_by_pics_number TXT (asc/desc)
        + sort_by_category TXT (asc/desc)
        + sort_by_author TXT (asc/desc)
        + sort_by_date TXT (asc/desc)

    Json answer example:

        {"page":1,"posts":[{"post_id":40,"author":{"author_id":35,"author_info":"info","user_id":35},"post_name":"name","post_create_date":"2021-02-17","post_category":{"category_id":15,"category_name":"donec","sub_categories":[37,46,58]}}}}},"post_text":"text","post_main_pic_id":18,"post_main_pic_url":"http://localhost:3000/pictures/18","post_pics":[{"pic_id":40,"pic_url":"http://localhost:3000/pictures/40"},{"pic_id":31,"pic_url":"http://localhost:3000/pictures/31"}],"post_tags":[{"tag_id":6,"tag_name":"eget"},{"tag_id":63,"tag_name":"quis"}]},{"post_id":16,"author":{"author_id":33,"author_info":"info","user_id":33},"post_name":"name","post_create_date":"2021-02-02","post_category":{"category_id":15,"category_name":"donec","sub_categories":[37,46,58]}}},"post_text":"text","post_main_pic_id":96,"post_main_pic_url":"http://localhost:3000/pictures/96","post_pics":[{"pic_id":114,"pic_url":"http://localhost:3000/pictures/114"},{"pic_id":421,"pic_url":"http://localhost:3000/pictures/421"}],"post_tags":[{"tag_id":29,"tag_name":"amet"},{"tag_id":67,"tag_name":"cras"}]}]}

8. Comment methods:
    1. To create comment:  
    POST comments/post_id INT  
    Query parameters:
    - comment_text TXT (max500char)
    - token TXT (user/admin token)
           
    2. To get one comment:  
    GET /comments/INT(comment_id)  

    Json answer example:
        
        {"comment_id":1001,"comment_text":"cool","post_id":16,"user_id":27}

    3. To get several comments for post:  
    GET /comments  
    Query parameters:  
    - post_id INT
    - page INT
    
    Json answer example:

        {"page":1,"post_id":17,"comments":[{"comment_id":971,"comment_text":"text","user_id":489},{"comment_id":952,"comment_text":"text","user_id":348}]}

    4. To update comment:  
    PUT /comments/comment_id INT  
    User should be comment author.  
    Query parameters:
    - comment_text TXT(max500char)
    - token TXT (user/admin token)

    Json answer example:
        
        {"comment_id":1001,"comment_text":"cool","post_id":16,"user_id":27}

    5. To deleteComment. 
    DELETE /comments/comment_id INT  
    User should be admin or comment/post author.  
    Query parameters:
    - token TXT (user/admin token)
           

# Logging

There are 4 logging levels from lowest to highest:

1. DEBUG
2. INFO
3. WARNING
4. ERROR

The logging level is specified in [Configuration](#2-configuration). The log will only display entries of this level and levels above.

# Project structure

Description of project sections:
1. App - module with main app functionality. Here actions are selected depending on the request. Has handler.
2. Methods - section, that contain modules with handlers from all methods. Module "Methods.hs" has handler, that combines all methods.  
    1. General methods handlers:
        1. Admin
        1. Author
        1. Category
        1. Comment
        1. Draft
        1. Picture
        1. Post
        1. Tag
        1. User  
    1. Common methods handlers(can be used in several general methods handlers):
        1. Auth
        1. Exist
        1. DeleteMany
        1. MakeCatResp
3. Psql - folder for work with database:
    1. "ToQuery" section - parsing db requests
    1. "Selecty.hs" - parsing db responses
    1. "Methods" folder - all IO methods for corresponding handlers.
3. Api - response and request parsing.
5. Conf - configuratuion parsing and db connection.
5. Logger - logging.
5. Oops - work with exceptions.
5. TryRead - some more parsing functions.
5. Types - some more project types.

# Tests

You can test app with:

    $ stack test

Modules, which has handlers, have unit-tests:
1. General methods handlers:
    1. Admin
    1. Author
    1. Category
    1. Comment
    1. Draft
    1. Picture
    1. Post
    1. Tag
    1. User  
1. Common methods handlers:
    1. Auth
    1. MakeCatResp