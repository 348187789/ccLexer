ccLexer
===========================
A lexer written in C++. 


How to use
---------------------------

		
		BOOST_AUTO_TEST_SUITE(s_example)


		BOOST_AUTO_TEST_CASE(t_example_regex_match)
		{
			using cc::regex_match;
			BOOST_CHECK_EQUAL(true,  regex_match("abb", "ab*"));
			BOOST_CHECK_EQUAL(true,  regex_match("int32_t", "[a-z_][a-z0-9_]*"));
			BOOST_CHECK_EQUAL(true,  regex_match("// this is a note", "////[^\n]*"));
		}

		BOOST_AUTO_TEST_CASE(t_example_lex)
		{
			using cc::lexer;
			using cc::lexIterator;
			lexer lex;
			const int tag_blank = lex.regist(" ");
			const int tag_nextline = lex.regist("\n");
			const int tag_id = lex.regist("[a-z_][a-z0-9_]*");
			const int tag_note1 = lex.regist("////[^\n]*");
			const int tag_note2 = lex.regist("///*[^/*]*(([^/*//](//)*)*(/*))+//");
			lex.compile();

			char* tags[] = {"tag_blank", "tag_nextline", "tag_id", "tag_note1", "tag_note2"};

			std::string str("hello var1//var var1\n /*/* note /**/");

			std::istringstream stream(str);
			auto iter = lex.getIterator(stream);

			auto printIterContent = [&tags]
			(lexIterator& iter)
			{
				std::cout<<"("
						 <<std::setw(12)<<tags[(*iter).second]
						 <<"  ,  "
						 <<"\""<<(*iter).first<<"\""
						 <<")"
						 <<std::endl;
			};

			while(iter)
			{
				printIterContent(iter);
				++iter;
			}
		}

		BOOST_AUTO_TEST_SUITE_END()

Output
---------------------------


		(      tag_id  ,  "hello")
		(   tag_blank  ,  " ")
		(      tag_id  ,  "var1")
		(   tag_note1  ,  "//var var1")
		(tag_nextline  ,  "
		")
		(   tag_blank  ,  " ")
		(   tag_note2  ,  "/*/* note /**/")
