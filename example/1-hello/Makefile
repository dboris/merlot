.PHONY: all clean test

MERLOT = ../../_build/default/bin/main.exe

all: hello.beam

hello.beam: hello.ml
	$(MERLOT) hello.ml

test: hello.beam
	@erl -noshell -eval 'io:format("add(2, 3) = ~p~n", [hello:add(2, 3)]), io:format("factorial(5) = ~p~n", [hello:factorial(5)]), io:format("fibonacci(10) = ~p~n", [hello:fibonacci(10)]), io:format("sum_list([1,2,3,4]) = ~p~n", [hello:sum_list([1,2,3,4])]), io:format("map_double([1,2,3]) = ~p~n", [hello:map_double([1,2,3])]), io:format("color_to_string(red) = ~p~n", [hello:color_to_string(red)]), io:format("area({circle, 2.0}) = ~p~n", [hello:area({circle, 2.0})]), io:format("safe_divide(10, 2) = ~p~n", [hello:safe_divide(10, 2)]), io:format("safe_divide(10, 0) = ~p~n", [hello:safe_divide(10, 0)]), io:format("reverse_list([1,2,3]) = ~p~n", [hello:reverse_list([1,2,3])]).' -s init stop

clean:
	rm -f *.core *.beam *.cmi erl_crash.dump
