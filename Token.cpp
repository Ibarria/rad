#include "Token.h"
#include <stdlib.h>

Token::~Token()
{
	if (type == STRING || type == IDENTIFIER) {
		if (pl.pstr) {
			free(pl.pstr);
			pl.pstr = nullptr;
		}
		type = EOF;
	}
}
