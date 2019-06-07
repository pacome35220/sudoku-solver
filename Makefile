##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

NAME	=	sudoku-solver

all:
	stack build --copy-bins --local-bin-path .

clean:
	@stack clean
	@$(RM) $(NAME).cabal

fclean:	clean
	@$(RM) $(NAME)

re:	fclean all
