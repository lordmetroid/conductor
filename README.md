# Conductor
A Strict Web Application Server

## Configuration
```erlang
{ip, "127.0.0.1"}.
{port, 80}.
{log_dir, 				"/website/log/"}.

{file_dir,				"/website/files/"}.

{program_dir,			"/website/programs/"}.
{model_dir,				"/website/programs/models/"}.
{view_dir,				"/website/programs/views/"}.
{controller_dir			"/website/programs/controllers/"}.

{programs, [
	{"/",				"index.erl"},
	{"/index.html",		"index.erl"}
]}.

```

## Execution
> erl -pa ebin/ -run conductor -conf priv/test.conf
