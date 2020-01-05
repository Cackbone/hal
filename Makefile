NAME		=	hal
BUILD_PATH	=	$(shell stack path --local-install-root)/bin/hal-exe


.PHONY: all clean

all:
	@stack build
	@cp $(BUILD_PATH) ./$(NAME)

clean:
	@stack clean

fclean: clean
	@rm -f $(NAME)

re: fclean all

