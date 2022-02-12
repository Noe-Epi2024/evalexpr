##
## EPITECH PROJECT, 2020
## B-FUN-400-PAR-4-1-funEvalExpr-noe.jais
## File description:
## Makefile
##

MAIN	:= app/Main.hs

SRC		:= src/Lib.hs

TESTSRC	:= test/Spec.hs

OBJ		:= $(MAIN:.hs=.o) $(MAIN:.hs=.hi)
OBJ		+= $(SRC:.hs=.o) $(SRC:.hs=.hi)
OBJ		+= $(TESTSRC:.hs=.o) $(TESTSRC:.hs=.hi)

NAME	:= funEvalExpr

all: $(NAME)

$(NAME): $(SRC) $(MAIN)
	stack build
	stack install --local-bin-path .

tests_run :
	stack test

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all debug clean fclean re