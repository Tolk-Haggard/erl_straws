-module(straws).

-export([
  straw/2,
  straw2/2,
  gen_buckets/4,
  gen_objects/2,
  map_objects/3,
  diff_maps/2,
  calc_bucket_load/1,
  calc_bucket_space/1
  ]).

-type bucket() :: {Name::string(), Weight::float()}.
-type mapped_object() :: {Object::string(), Bucket::string()}.

-spec calc_bucket_load(Map::[mapped_object()]) -> ok.
calc_bucket_load(Map) ->
  TotalObjects = length(Map),
  BucketCounts = lists:foldl(fun({_, Bucket}, Acc) ->
                                 case proplists:get_value(Bucket, Acc) of
                                   undefined -> [{Bucket, 1} | Acc];
                                   Count ->
                                     Deleted = proplists:delete(Bucket, Acc),
                                     [{Bucket, 1 + Count} | Deleted]
                                 end
                             end, [], Map),
  lists:foreach(fun({Bucket, Count}) ->
                    io:format("Bucket: ~p Had ~.1f % of the objects~n", [Bucket, Count / TotalObjects * 100])
                end, lists:keysort(2, BucketCounts)).

-spec calc_bucket_space(Buckets::[bucket()]) -> ok.
calc_bucket_space(Buckets) ->
  TotalSpace = lists:foldl(fun({_, X}, Acc) -> X + Acc end, 0, Buckets),
  lists:foreach(fun({Bucket, Space}) ->
                    io:format("Bucket: ~p Has ~.1f % of the total space~n", [Bucket, Space / TotalSpace * 100])
                end,
                lists:keysort(2, Buckets)).

-spec diff_maps(Map1::[mapped_object()], Map2::[mapped_object()]) -> ok.
diff_maps(Map1, Map2) ->
  lists:foreach(fun({Object, OldBucket}) ->
                    case proplists:get_value(Object, Map2) of
                      OldBucket -> no_op;
                      NewBucket -> io:format("Object: ~p Moved From: ~p To: ~p~n", [Object, OldBucket, NewBucket])
                    end
                end,
                Map1).

-spec map_objects([bucket()], [Object::string()], Algorithm::fun()) -> [mapped_object()].
map_objects(Buckets, Objects, Algorithm) ->
  lists:map(fun(Object) ->
                Bucket = Algorithm(Buckets, Object),
                {Object, Bucket}
            end,
            Objects).


-spec gen_objects(Prefix::string(), Count::float()) -> [string()].
gen_objects(Prefix, Count) ->
  lists:map(fun(Suffix) ->
                Prefix ++ integer_to_list(Suffix)
            end,
            lists:seq(1, Count)).

-spec gen_buckets(Prefix::string(), Count::float(), Start::float(), Step::float()) -> [bucket()].
gen_buckets(Prefix, Count, Start, Step) ->
  {_, Buckets} = lists:foldl(fun(Suffix, {Current, Acc}) ->
                                 case random:uniform(3) of
                                   1 ->
                                     Current1 = Current + Step,
                                     {Current1 , [{Prefix ++ integer_to_list(Suffix), Current1} | Acc]};
                                   _ ->
                                     {Current , [{Prefix ++ integer_to_list(Suffix), Current} | Acc]}
                                 end
                             end,
                             {Start, []},
                             lists:seq(1, Count)),
  lists:reverse(Buckets).

-spec straw([bucket()], Object::string()) -> Name::string().
straw(Buckets, Object) ->
  StrawFun = fun({Name, ScalingFactor}, {MaxX, MaxName}) ->
               <<_:18/binary, Draw:16/integer>> = crypto:hash(sha, Name ++ Object),
               X = Draw * ScalingFactor,
               case X > MaxX of
                 true  -> {X, Name};
                 false -> {MaxX, MaxName}
               end
             end,
  Buckets1 = calc_scaling_factors(Buckets),
  {_, ChosenName} = lists:foldl(StrawFun, {-1, undefined}, Buckets1),
  ChosenName.

calc_scaling_factors(Buckets) ->
  calc_scaling_factors1(lists:reverse(lists:keysort(2, Buckets)), 1, [], 0, 0).

calc_scaling_factors1([{Bucket, _Size}], Straw, Acc, _LastSize, _SizeBelow) ->
  ScalingFactor = Straw * 16#10000,
  [{Bucket, ScalingFactor} | Acc];
calc_scaling_factors1([{Bucket, Size} | Tail], Straw, Acc, LastSize, SizeBelow) ->
  ScalingFactor = Straw * 16#10000,
  {_, NextSize} = hd(Tail),
  SizeBelow1 = SizeBelow + ((Size - LastSize) * (length(Tail) + 1)),
  WNext = length(Tail) * (NextSize - Size),
  PBelow = SizeBelow1 / (SizeBelow1 + WNext),
  Straw1 = Straw * math:pow(1.0 / PBelow, 1.0 / length(Tail)),
  calc_scaling_factors1(Tail, Straw1, [{Bucket, ScalingFactor} | Acc], Size, SizeBelow1).

-spec straw2([bucket()], Object::string()) -> Name::string().
straw2(Buckets, Object) ->
  StrawFun = fun({Name, Weight}, {MaxX, MaxName}) ->
               <<_:18/binary, Draw:16/integer>> = crypto:hash(sha, Name ++ Object),
               X = (math:log((Draw + 1) / 65536) ) / Weight,
               case X > MaxX of
                 true  -> {X, Name};
                 false -> {MaxX, MaxName}
               end
             end,
  {_, ChosenName} = lists:foldl(StrawFun, {- 16#1000000000001, undefined}, Buckets),
  ChosenName.
