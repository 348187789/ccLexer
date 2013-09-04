ccLexer
===========================
A lexer written in C++. 


How to use
---------------------------

		#include <iostream>
		#include "ccLexer.h"

		int main()
		{
			cc::lexer myLexer;

			const int tag_blank = myLexer.regist(" ");
			const int tag_set   = myLexer.regist("=");
			const int tag_id    = myLexer.regist("([a-z]|_)([a-z0-9]|_)*");
			const int tag_num   = myLexer.regist("(/+|-)?(0|[1-9][0-9]*)(.[0-9]*)?");
			
			myLexer.endRegist();

			assert(tag_blank == 0);
			assert(tag_set   == 1);
			assert(tag_id    == 2);
			assert(tag_num   == 3);

			std::string toMatch("float var1=1.8");

			auto iter = myLexer.getIterator(&toMatch);

			auto printContent = []
			(decltype(iter)& inputIter)
			{
				std::cout<<'('
						<<std::setw(5)<<(*inputIter).first
						<<", "
						<<std::setw(5)<<(*inputIter).second
						<<')'<<std::endl;
			};
			while(iter)
			{
				printContent(iter);
				++iter;
			} 
					  
		}

Output

----------------------------

		(float,     2)
		(     ,     0)
		( var1,     2)
		(    =,     1)
		(  1.8,     3)
