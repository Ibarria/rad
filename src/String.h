#pragma once
#include "mytypes.h"
#include "Allocator.h"

class String
{
	Allocator* allocator;
	char* data;
	u64 data_size;

	bool resize(u64 new_size);
	void free_resources();
public:
	String(const char *s = nullptr, Allocator *a = nullptr);
	String(const String& str);
	String& operator = (const char* s);
	String& operator = (const String& str);
	bool operator == (const char* s);
	bool operator == (const String& str);
	bool operator != (const char* s);
	bool operator != (const String& str);

	~String();
};

