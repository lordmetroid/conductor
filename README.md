# Conductor
A Strict Web Application Server

## Configurating a web application
Configuration of the Conductor web application server is needed in order to
run a server. See the below example configuration and explanation:

```erlang
%% Application listening host and port
{ip, "127.0.0.1"}.
{port, 8000}.

%% Application Logs
{log_dir,               "/website/log/"}.

%% Application files
{file_dir,              "/website/files/"}.

%% Application programs
{program_dir,           "/website/programs/"}.
{programs, [
	{"/",               "index.erl"},
	{"/index.html",     "index.erl"}
]}.

%% Application program resources
{model_dir,             "/website/programs/models/"}.
{view_dir,              "/website/programs/views/"}.
{controller_dir,        "/website/programs/controllers/"}.

```
The configuration file consist of a number of value-data pairs.

* Application listening host and port is defined by the _ip_ and _port_ 
value-data pairs. Notice that the host defined in the _ip_ value-data pair 
is a __string__ while the _port_ value-data pair is an __integer__.

* Application logs specifies the directory where logs will written.

* Application files specifies the directory where files such as images, 
javascripts, css files, etc. for the web application are stored.

* Application programs are the programs which are available to the users
of the web application. The _program_dir_ value-data pair specifies the 
directory where the programs are stored and the available programs are 
specified by the _programs_ value-data pair. The _programs_ is a __list__ 
of sets of two values. The absolute website search path and the corresponing 
program to be executed on a request for that search path.

* Application program resources specifies the directories where resources 
available to a program are stored.


## Execution
> erl -pa ebin/ -run conductor -conf website.conf

Applications consist of four components. A Program and associated models, 
views and controllers.

* The program component is an erlang module that is responsible for the general 
execution of a request. The program contains calls to various controllers.

* A controller component is an erlang module that has a specific function, 
for example, render a part of the page, add data to a database, etc. 
The controller contains calls to various model and view components and 
the results from the calls are managed by the controller.

* A model component is an erlang module that communicates with sources of 
volatile data such as for example a database. The model is responsible for 
fetching, updating and adding data.

* A view component is a scripted file in one of the supported languages
specified by the installed view compilers. The view must only render received 
data and never make decisions on what data to render. All decision making is 
done by the controller.
