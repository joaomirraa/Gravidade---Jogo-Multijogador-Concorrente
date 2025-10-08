%% PVectors module for vector math functions in Erlang
%-module(pvectors).
-export([pvector_dist/2, pvector_sub/2, pvector_add/2, set_magnitude/2, pvector_limit/2]).

%% Define the PVector record for vector math
-record(pvector, {x :: float(), y :: float()}).

%% Type definition for pvector()
-type pvector() :: #pvector{x :: float(), y :: float()}.

%% Calculate the Euclidean distance between two PVectors
-spec pvector_dist(pvector(), pvector()) -> float().
pvector_dist(Vec1, Vec2) ->
    DistanceX = Vec1#pvector.x - Vec2#pvector.x,
    DistanceY = Vec1#pvector.y - Vec2#pvector.y,
    math:sqrt(DistanceX * DistanceX + DistanceY * DistanceY).

%% Subtract two PVectors
-spec pvector_sub(pvector(), pvector()) -> pvector().
pvector_sub(Vec1, Vec2) ->
    #pvector{x = Vec1#pvector.x - Vec2#pvector.x,
             y = Vec1#pvector.y - Vec2#pvector.y}.

%% Add two PVectors
-spec pvector_add(pvector(), pvector()) -> pvector().
pvector_add(Vec1, Vec2) ->
    #pvector{x = Vec1#pvector.x + Vec2#pvector.x,
             y = Vec1#pvector.y + Vec2#pvector.y}.

%% Calculate the magnitude of a PVector
-spec pvector_magnitude(pvector()) -> float().
pvector_magnitude(Vec) ->
    math:sqrt(Vec#pvector.x * Vec#pvector.x + Vec#pvector.y * Vec#pvector.y).

%% Normalize a PVector
-spec normalize(pvector()) -> pvector().
normalize(Vec) ->
    Magnitude = pvector_magnitude(Vec),
    #pvector{x = Vec#pvector.x / Magnitude, y = Vec#pvector.y / Magnitude}.

%% Set the magnitude of a PVector
-spec set_magnitude(pvector(), float()) -> pvector().
set_magnitude(Vec, NewMagnitude) ->
    NormalizedVec = normalize(Vec),
    #pvector{x = NormalizedVec#pvector.x * NewMagnitude,
             y = NormalizedVec#pvector.y * NewMagnitude}.

%% Limit the magnitude of a PVector to a given value
-spec pvector_limit(pvector(), float()) -> pvector().
pvector_limit(Vec, Limit) ->
    Magnitude = pvector_magnitude(Vec),
    case Magnitude > Limit of
        true -> set_magnitude(Vec, Limit);
        false -> Vec
    end.