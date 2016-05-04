
-module(elsa_router).

-export([start/0
         ]).

start() ->
    elsa_kernel_handler_v2:start(),
    {ok, _} = cowboy:start_http(elsa_server, 100, [{port, 8080}],
        [{env, [{dispatch, dispatch('_')}]}]
    ),
    lager:info("Router started").

dispatch(Host) ->
  Routes = routes(),
  lager:info("Compiling routes: ~p", [routes()]),
  cowboy_router:compile([{Host, Routes}]).

routes() ->
  v1_routes().

v1_routes() ->
  version("v1", [
    {"/task", v1_elsa_tasks_handler, []},
    {"/task/:id", v1_elsa_task_handler, []}
  ]) ++ version("v1", [
    {"/service", v1_elsa_services_handler, []},
    {"/service/:service_name", v1_elsa_service_handler, []}
  ]) ++ version("v1", [
    {"/service/:service_name/:service_version/instance", v1_elsa_instances_handler, []},
    {"/service/:service_name/:service_version/instance/:instance_id", v1_elsa_instance_handler, []}
  ]) ++ version("v1", [
    {"/service/:service_name/:service_version/instance/:instance_id/thread", v1_elsa_threads_handler, []},
    {"/service/:service_name/:service_version/instance/:instance_id/thread/:thread_id", v1_elsa_thread_handler, []}
  ]).

version(Version, Routes) when is_list(Routes) ->
  [version(Version, Route) || Route <- Routes];
version(Version, {Route, Handler, Options}) ->
  {"/" ++ Version ++ Route, Handler, Options}.
