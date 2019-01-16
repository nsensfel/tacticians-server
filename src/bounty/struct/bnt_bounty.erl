-module(bnt_bounty).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id() :: ataxia_id:type().
-type job() :: {atom(), atom(), list(any())}.

-record
(
   bounty,
   {
      deadline :: ataxia_time:type(),
      user :: ataxia_security:user(),
      job :: job()
   }
).

-type type() :: #bounty{}.

-export_type([type/0, id/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export
(
   [
      generate/4,
      resolve/0
   ]
).

-export
(
   [
      get_deadline/1,
      get_user/1,
      get_job/1
   ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXPORTED FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec generate
   (
      ataxia_security:user(),
      ataxia_time:type(),
      ataxia_time:type(),
      job()
   )
   -> {'ok', ataxia_id:type()}.
generate (User, NotBefore, Deadline, Job) ->
   Janitor = ataxia_security:janitor(),
   JanitorOnly = ataxia_security:allow_only(Janitor),
   JanitorAndUser = ataxia_security:add_access(User, JanitorOnly),

   Bounty =
      #bounty
      {
         deadline = Deadline,
         user = User,
         job = Job
      },

   {ok, BountyID} =
      ataxia_client:add
      (
         bounty_db,
         JanitorAndUser,
         JanitorAndUser,
         ataxia_lock:locked(User, NotBefore),
         Bounty
      ),

   {ok, BountyID}.

-spec resolve () -> type().
resolve () ->
   Lock = ataxia_lock:locked(ataxia_security:admin(), ataxia_time:in(60)),

   {ok, Bounty, BountyID} =
      ataxia_client:update_and_fetch_any
      (
         bounty_db,
         ataxia_security:janitor(),
         ataxic:update_lock(ataxic:constant(Lock)),
         ataxic:constant(true)
      ),

   {Module, Function, Params} = Bounty#bounty.job,

   erlang:apply(Module, Function, [BountyID|Params]).

-spec get_deadline (type()) -> ataxia_time:type().
get_deadline (Bounty) -> Bounty#bounty.deadline.

-spec get_user (type()) -> ataxia_security:user().
get_user (Bounty) -> Bounty#bounty.user.

-spec get_job (type()) -> job().
get_job (Bounty) -> Bounty#bounty.job.
