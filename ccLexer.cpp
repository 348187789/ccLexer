#include "stdafx.h"

#include <cassert>

#include <boost/timer.hpp>

#include <mutex>
#include <memory>
#include <utility>
#include <exception>
#include <functional>

#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <tuple>
#include <vector>
#include <queue>
#include <string>

#include "ccLexer.h"


		/////////////////////////////////////////////////////////////////debug
double timeOfSomeOperation;

struct countTime
{
	boost::timer t;
	bool counted;
	
	countTime() : counted(false){}

	void count(){counted = true; timeOfSomeOperation += t.elapsed();}

	~countTime()
	{
		if(!counted)
		{
			timeOfSomeOperation += t.elapsed();
		}
	}
};


/******************************************************************/
/*                         class pool                             */
/******************************************************************/

namespace cc
{

class onScopeEnd
{
public:
	onScopeEnd(const std::function<void(void)>& inputFunction)
		: funcToRunOnScopeEnd(inputFunction)
	{}
	~onScopeEnd()
	{
		funcToRunOnScopeEnd();
	}
private:
	std::function<void(void)> funcToRunOnScopeEnd;
};
	

template<class T, unsigned int N = 20>
class simplePool
{
public:

    simplePool()
        : blockBegin( malloc(sizeof(T) * N) ) 
        , end( static_cast<T*>(blockBegin) + N )
    {
        if(blockBegin == nullptr)
        {
            throw std::bad_alloc();
        }
        itemIter = static_cast<T*>(blockBegin);
    }

    ~simplePool()
    {
        if(blockBegin != nullptr)
        {
            for(T* destructIter = itemIter - 1; 
                destructIter >= static_cast<T*>(blockBegin); 
                --destructIter)
            {
                destruct(destructIter);
            }
            free(blockBegin);
        }
        next.reset();
    }

    bool full()
    {
        return itemIter == end;
    }

    T* getItemPtr()
    {
        if(full())
            throw std::exception("get memory from full simplePool\n");
        construct(itemIter);
        return itemIter++;
    }


public:
    std::shared_ptr<simplePool<T, N> > next;

private:
    void construct(T* p)
    {
        ::new (static_cast<void*>(p)) T (); // placement new
    }
    void destruct(T* p)
    {
        p->~T();
    }

    simplePool(const simplePool&);
    simplePool& operator=(const simplePool&);
    void * const blockBegin;
    T * const end;
    T * itemIter;

};// class simplePool



template<class T, unsigned int N = 20>
class pool
{
public:

    pool(){}

    pool(pool&& other)
    {
        last = std::move(other.last);
        first = std::move(other.first);// 注意这里顺序不能变，否则会把前n-1个块全都析构掉
    }

    ~pool(){}

    void operator=(pool&& other)
    {
        last = std::move(other.last);
        first = std::move(other.first);// 注意这里顺序不能变，否则会把前n-1个块全都析构掉
    }

    T* get()
    {
        if(!first)
        {
            first.reset(new simplePool<T, N>);
            last = first;
        }
        else if(first->full())
        {
            std::shared_ptr<simplePool<T, N> > tmp = first;
            first.reset(new simplePool<T, N>);
            first->next = tmp;
        }

        return first->getItemPtr();
    }

    void combine(pool&& other)
    {
        if(last)
        {
            last->next = other.first;
            last = other.last;
        }
        else if(other.last)
        {
            first = other.first;
            last  = other.last;
        }
        // or do nothing
        other.first.reset();
        other.last.reset();
    }

    void swap(pool& other)
    {
        std::swap(first, other.first);
        std::swap(last,  other.last);
    }

private:
    pool(const pool&);
    pool& operator=(const pool&);

    std::shared_ptr< simplePool<T, N> > first;
    std::shared_ptr< simplePool<T, N> > last;

}; // class pool

} // namespace cc

namespace cc
{
	template<class T1, class T2>
	inline bool equal(const T2& a, const T1& b)
	{
		return a == b;
	}
}

/******************************************************************/
/*                         regexParser                            */
/******************************************************************/

namespace ccDetail
{


void getIsRegexSpecialCharArry(const char*& ret) 
{
    static char specialCharArry[128];
    specialCharArry[static_cast<unsigned int>('|')] = 1;
    specialCharArry[static_cast<unsigned int>('.')] = 1;
    specialCharArry[static_cast<unsigned int>('*')] = 1;
    specialCharArry[static_cast<unsigned int>('+')] = 1;
    specialCharArry[static_cast<unsigned int>('?')] = 1;
    specialCharArry[static_cast<unsigned int>('(')] = 1;
    specialCharArry[static_cast<unsigned int>(')')] = 1;
    specialCharArry[static_cast<unsigned int>('[')] = 1;
    specialCharArry[static_cast<unsigned int>(']')] = 1;
    specialCharArry[static_cast<unsigned int>('{')] = 1;
    specialCharArry[static_cast<unsigned int>('}')] = 1;
    specialCharArry[static_cast<unsigned int>('\\')] = 1;
    specialCharArry[static_cast<unsigned int>('/')] = 1;
    ret = specialCharArry;
}

std::once_flag getIsRegexSpecialCharArryFlag;
static const char* isSpecialCharArry = nullptr;
bool isSpecialChar(char c)
{
    std::call_once(
        getIsRegexSpecialCharArryFlag, 
        getIsRegexSpecialCharArry,
        std::ref(isSpecialCharArry)
        ); // 这里默认是按值传输的，如果要按引用传值，需要使用std::ref();
    return isSpecialCharArry[static_cast<unsigned int>(c)] == 1;
}


class regex_exception : public std::exception
{
public:
    regex_exception(const char* message)
        : std::exception(message)
    {
        place = 0;
        atChar = '\0';
    }
    void setPlaceAndChar(int _place, char _atChar)
    {
        place = _place;
        atChar = _atChar;
    }
    std::string detail() const
    {
        return std::string(
            std::string("at index: ") + std::to_string(place) +
            std::string("\tunexpected: ") + atChar);
    }
private:
    int place;
    char atChar;
};


void regex_assert(bool expr, const char* message)
{
    if(!expr)
    {
        throw regex_exception(message);
    }
}


struct charRange
{
    charRange(char cBegin, char cEnd)
    {
        rangeConstruct(cBegin, cEnd);
    }
    charRange(char singleChar)
    {
        rangeConstruct(singleChar, singleChar);
    }

    char begin()
    {
        return rangeBegin;
    }
    char end()
    {
        return rangeEnd;
    }
private:
    void rangeConstruct(char cBegin, char cEnd)
    {
        regex_assert(
            (cEnd >= cBegin), "valid charRange");
        rangeBegin = cBegin;
        rangeEnd   = cEnd;
    }
    char rangeBegin;
    char rangeEnd; // end in range = true

}; // class charRange

class numRange // TODO with "x{0-9}"" 
{
};



// nfa 节点保证 首节点不被其他节点指向， 尾节点不指向 其他节点
// 连接两个nfa 则只需，swap(a.end, b.first)
// 则a.first 与 b.end 组成 新nfa
// a.end被遗弃


struct nfaState
{
//  数据成员
    std::vector<nfaState*> nextStates;
    std::vector<nfaState*> epsiStates;
//  成员函数
    // 默认构造函数

    void swap(nfaState& other)
    {
        nextStates.swap(other.nextStates);
        epsiStates.swap(other.epsiStates);
    }

    void linkToState(nfaState* next)
    {
        epsiStates.push_back(next);
    }

    void linkToState(nfaState* next, charRange cRange)
    {
        regex_assert(cRange.begin() != 127,"wrong char range, cannot use char(127)\n");
        assert(next);
        if(nextStates.empty()) // 性能不一定变好
        {
            nextStates.resize(127, nullptr);
        }
        for(char c = cRange.begin(); c <= cRange.end(); ++c)
        {
            nextStates[static_cast<unsigned int>(c)] = next;
        }
    }
};


struct nfa
{
    nfaState* pBegin;
    nfaState* pEnd;
    nfa(nfaState* pBeginArg = nullptr, nfaState* pEndArg = nullptr)
        : pBegin(pBeginArg)
        , pEnd(pEndArg)
    {}
    operator bool() const
    {
        bool ret = (pBegin != nullptr);
        ret = ret && (pEnd != nullptr);
        assert((pBegin != nullptr) == (pEnd != nullptr));
        return ret;
    }
};


/**********************************************/
/*               getNfa                       */
/**********************************************/

std::tuple< nfa, cc::pool<nfaState> > getNfa(const std::string& str)
{
    cc::pool<nfaState> nfaPool;
    auto strBegin = str.begin();
    auto strIter  = str.begin();
    auto strEnd   = str.end();

    auto get = [&]{  return nfaPool.get();    };

    /***********************/
    /*       nfa构造       */
    /***********************/

    auto constructByCharRange = [&]
    (charRange cRange)->nfa
    {
        nfa newNfa(get(), get());
        assert(newNfa);
        newNfa.pBegin->linkToState(newNfa.pEnd, cRange);
        return newNfa;
    };

    auto orLink = [&]
    (nfa lhs, nfa rhs) -> nfa
    {
        assert(lhs && rhs);
        nfa newNfa(get(), get());
        newNfa.pBegin->linkToState(lhs.pBegin);
        //newNfa.pEnd->linkToState(rhs.pBegin); // bug fix
        newNfa.pBegin->linkToState(rhs.pBegin);
        lhs.pEnd->linkToState(newNfa.pEnd);
        rhs.pEnd->linkToState(newNfa.pEnd);
        return newNfa;
    };

    auto andLink = [&]
    (nfa lhs, nfa rhs) -> nfa
    {
        assert(lhs && rhs);
        lhs.pEnd->swap(*(rhs.pBegin));
        return nfa(lhs.pBegin, rhs.pEnd);
    };

    auto repeat = [&]
    (nfa inputNfa) -> nfa 
    {
        assert(inputNfa);
        nfa newNfa(get(), get());
        assert(newNfa);
        newNfa.pBegin->linkToState(inputNfa.pBegin);
        inputNfa.pEnd->linkToState(newNfa.pEnd);
        inputNfa.pEnd->linkToState(inputNfa.pBegin);
        inputNfa.pBegin->linkToState(inputNfa.pEnd);
        return newNfa;
    };

    auto repeatOnceOrMore = [&]
    (nfa inputNfa) -> nfa 
    {
        assert(inputNfa);
        nfa newNfa(get(), get());
        newNfa.pBegin->linkToState(inputNfa.pBegin);
        inputNfa.pEnd->linkToState(newNfa.pEnd);
        // 比上面的少一句 link input.begin to input.end
        inputNfa.pEnd->linkToState(inputNfa.pBegin);
        return newNfa;
    };

    auto onceOrNot = [&]
    (nfa inputNfa) -> nfa
    {
        assert(inputNfa);
        inputNfa.pBegin->linkToState(inputNfa.pEnd);
        return inputNfa;
    };


    /***********************/
    /*       regex读取     */
    /***********************/

    auto readCharRange = [&]
    () -> charRange 
    {    //   id = ([a-zA-Z]|_) [1-9a-zA-Z]*
        // 暂时只支持 [a-zA-Z]形式
        if(isSpecialChar(*strIter))
            return charRange('\0'); // 返回时会被检测 *strIter == ']'
        char first = *strIter++;
        regex_assert(*strIter == '-', "valid char range");
        ++strIter;
        regex_assert(*strIter != char(0)  , "valid char range");
        char end = *strIter;
        ++strIter;
        // 调用函数检查 *strIter
        return charRange(first, end);

    }; // regex::readCharRange()


    // 因为要递归，要写成匿名函数,而且不能使用auto
    std::function<nfa()> readNfa = [&]
    () -> nfa
    {
        using cc::equal;
        nfa preNfa(nullptr, nullptr);
        nfa thisNfa(nullptr, nullptr);

        auto storePre = [&]
            {
                if(thisNfa && preNfa)
                {
                    preNfa = andLink(
                        preNfa, thisNfa);
                }
                else if(thisNfa)
                {
                    std::swap(preNfa, thisNfa);
                }
                thisNfa.pBegin = nullptr;
                thisNfa.pEnd   = nullptr;
            };
        
        for(; strIter != strEnd; ++strIter)
        {
            if(!isSpecialChar(*strIter))
            {
                storePre();
                thisNfa = constructByCharRange(charRange(*strIter));
            }
            else if(equal(*strIter, '|'))
            {
                // | 优先级最低（貌似是这样的）， 所以作用于前面所有的sd上
                // 而不是像*+仅仅作用在前一个字符上
                storePre();
                ++strIter;
                nfa orNfa = readNfa();
                // 注意此处调用了readNfa, 
                // strIter已经指向了下一个要读的字符
                // 在readNfa内调用自身(readNfa，一般指在for循环内)
                // 几乎每次都要--strIter来抵消for里的++strIter
                // 使此字符能被正确处理
                --strIter;

                assert(orNfa);
                preNfa = orLink(preNfa, orNfa); 
                // 为了保持正常状态(*+）等能正常操作于thisNfa;
                // 将状态恢复至thisNfa
                thisNfa = preNfa; 
                preNfa.pBegin = nullptr;
                preNfa.pEnd   = nullptr;
            }
            else if(equal(*strIter, '.'))
            {
                storePre();
                thisNfa = constructByCharRange( charRange(char(1), char(126)) ); 
                                            // 127是char的最大值，会导致range求值无限循环
            }
            else if(equal(*strIter, '*'))
            {   // TODO : 下面的改为 repeat(sd, numRange(0, 1))等
                regex_assert(thisNfa, "illegal '*' without preRegex");
                thisNfa = repeat(thisNfa);
            }
            else if(equal(*strIter, '+'))
            {
                regex_assert(thisNfa, "illegal '+' without preRegex");
                thisNfa = repeatOnceOrMore(thisNfa);
            }
            else if(equal(*strIter, '?'))
            {
                regex_assert(thisNfa, "illegal '?' without preRegex");
                thisNfa = onceOrNot(thisNfa);
            }
            else if(equal(*strIter, '('))
            {
                storePre();
                ++strIter;
                thisNfa = readNfa();
                assert(thisNfa); //readNfa 在返回时确保返回的是有效sd， 
                regex_assert(strIter!= strEnd && equal(*strIter, ')'), "unmatched '('\n");
            }
            else if(equal(*strIter, ')'))
            {
                break;
            }
            else if(equal(*strIter, '['))
            {
                storePre();
                thisNfa = nfa(nfaPool.get(), nfaPool.get());
                assert(thisNfa);
                ++strIter; // 每一个readXX()程序都直接从*strIter读起
                           // 调用者在函数返回后也是直接判断*strIter是不是指定终结符
                           // readXX()中还要持续检查'\0'
                charRange cRange = readCharRange();
                thisNfa.pBegin->linkToState(thisNfa.pEnd, cRange);
                while(!isSpecialChar(*strIter))
                {
                    cRange = readCharRange();
                    thisNfa.pBegin->linkToState(thisNfa.pEnd, cRange);
                }
                regex_assert(strIter!= strEnd && equal(*strIter, ']'), "unmatched '['\n");
            }
            else if(equal(*strIter, ']'))
            {
                break;
            }
            else if(equal(*strIter, '{'))
            {
                regex_assert(false, "operator {} not supported");
                //numRange nr = readNumRange(); // unfinished
                //regex_assert(equal(*strIter, '}'), "unmatched }\n");
                //thisNfa = repeat(thisNfa, nr);
                regex_assert(strIter!= strEnd && equal(*strIter, '}'), "unmatched '{'\n");
            }
            else if(equal(*strIter, '}'))
            {
                break;
            }
            else if(equal(*strIter, '/'))
            {   // TODO 增加更多的转义字符 如/d
                storePre();
                strIter++;
                regex_assert(isSpecialChar(*strIter), " illegal escape character"); //非法的转义字符
                char special = *strIter;
                thisNfa = constructByCharRange(charRange(special));
            }
            else if(equal(*strIter, '\\'))
            {
                storePre();
                strIter++;
                regex_assert(isSpecialChar(*strIter), " illegal escape character"); //非法的转义字符
                char special = *strIter;
                thisNfa = constructByCharRange(charRange(special));
            }
        }
        storePre();
        if(!preNfa)
            regex_assert(false, "empty regex\n");
        return preNfa;
    }; // regex::readNfa()


    nfa result = readNfa();
    try{
        regex_assert(strIter == strEnd, 
            "unexcept terminator char");
    }
    catch(const regex_exception e)
    {
        regex_exception detailEx(e);
        detailEx.setPlaceAndChar(
            strIter - strBegin, *strIter);
        throw detailEx;
    }
    return std::make_tuple(std::move(result), std::move(nfaPool)); 
}




//typedef std::unordered_set<nfaState*> closure;
// 这里使用unordered_set会assertin fail

typedef std::set<nfaState*> closure;
//typedef std::set<std::pair<int, nfaState*> > closure;

closure getClosure(nfaState* inputNfaState)
{
	// 实际代码中使用的是使用是包装后的getClosureInCache
    closure result;
    assert(inputNfaState);
    if(nullptr == inputNfaState)
    {
        return result; //注意空闭包处理 TODO
    }

    auto insertToResult = [&]
    (nfaState* inputState)
    {
        auto success = result.insert(inputState).second;
        assert(success);
        if(!success)
        {
            throw std::exception();
        }
    };

    insertToResult(inputNfaState);

    std::queue<nfaState*> toTraverse;
    toTraverse.push(inputNfaState);

    while(!toTraverse.empty())
    {
        nfaState* thisState = toTraverse.front();
        toTraverse.pop();

        for(nfaState* pState : thisState->epsiStates)
        {
            assert(pState);
            if(!result.count(pState))
            {
                insertToResult(pState);
                toTraverse.push(pState);
            }
        }
    }
    return result;

}


/******************************************************************/
/*                         nfa -> dfa                             */
/******************************************************************/


// 每次查询的闭包和最后dfa使用到的闭包不是同一个集合，
// 所有查询过的单节点对应的闭包存放如 closureChache里面， 由closureCacheMap查询
//  closureCache type : std::set<closure>
//  closureCacheMap type : std::map<nfaState*, closure*>
//  getClosureCache type : function<const closure&(nfaState)>

// 用于形成 dfa 的闭包存放在 sMove 中
// sMoveType : std::map< closure, std::pair<dfaState, int> >
// 其中 int 是用于生成 dfa的ID

class hashOfClosure
{
public:
std::size_t operator()(const closure& inputClosure) const 
	{
		size_t hashValue = 0;
		for(nfaState* pState : inputClosure)
		{
			hashValue =    unsigned int(pState)+ unsigned int(pState) & 31 * 33 ;
		}
		return hashValue;
	};

};

//

std::vector<dfaState> getDfa(const std::vector<nfa>& inputNfaVec)
{
    // inputVec 中的列出了优先级从高到低的regex对应的nfa，大小为1时可直接用于regex_match
    // 暂时使用这种方案，对dfa状态的数量可能有所影响， to改进

    
    assert(inputNfaVec.size() > 0);
    

    /**************************************************/
    /*             lamda函数                            */
    /**************************************************/
    //std::set<closure> closureCache;
    //std::unordered_set<closure, hashOfClosure> closureCache;
    //std::map<nfaState*, const closure*> closureCacheMap;
	std::unordered_map<nfaState*, closure> closureCache;
    typedef decltype(closureCache) closureCacheType;
    //typedef decltype(closureCacheMap) cacheMapType;

    auto getClosureInCache = [closureCache] // 这里使用mutable让cache成为function成员，不被外部改变
    (nfaState* inputNfaState) mutable -> const closure&
    {
        closureCacheType::iterator resultIterator = closureCache.find(inputNfaState);
        if(resultIterator != closureCache.end()) 
        // 如果map里有statePtr信息 ， 直接查询返回
        {
            return (*resultIterator).second;
        }
        else
        {
            closure resultClosure = getClosure(inputNfaState);
            auto resultOfInsertToCache = closureCache.insert(
				closureCacheType::value_type(std::move(inputNfaState), std::move(resultClosure)) );
            assert(resultOfInsertToCache.second);
			return (*resultOfInsertToCache.first).second;
            
        }
    };


    int idForLambda = -1;
    auto getId= [idForLambda]
    () mutable -> int
    {
        return ++idForLambda;
    };
    
    // 核心数据

	

    std::unordered_map<closure, std::pair<dfaState, int>, hashOfClosure> sMove;
	sMove.max_load_factor(2);//由1->2，大数据量下总时间 2.3->2.0
	// std::map<closure, std::pair<dfaState, int> sMove;
    typedef decltype(sMove) sMoveType;
    typedef std::pair<dfaState, int> sMoveContentType;



	// getTerminalId 使用新算法 在该函数性能提高40% 总时间降低8%
	std::unordered_map<nfaState*, int> terminalIdMap;
	typedef decltype(terminalIdMap) terminalIdMapType;

	auto terminalIdOfNfaStateMaker = 0;
	for(const nfa& n : inputNfaVec)
	{
		terminalIdMap.insert(terminalIdMapType::value_type(n.pEnd, terminalIdOfNfaStateMaker));
		++terminalIdOfNfaStateMaker;
	}

	auto getTerminalId = [&inputNfaVec, &terminalIdMap]
    //  id为 nonterminalFlagInTerminalId 则表示该节点不是终结节点， 其它当id = i时则表示该节点是最大优先级为 i 的终结节点
    (const closure& inputClosure) -> int
    {
		assert(nonterminalFlagOfDfaState > 1000000); // 算法需要flag大于所有正常id
		int maxPriorityTerminalId = nonterminalFlagOfDfaState;
		for(nfaState* inputNfaStatePtr : inputClosure)
		{
			int thisTerminalId = nonterminalFlagOfDfaState;
			auto findIter = terminalIdMap.find(inputNfaStatePtr); 
			if(findIter != terminalIdMap.end())
			{
				thisTerminalId = (*findIter).second;
			}
			if(thisTerminalId < maxPriorityTerminalId) // nonterminalFlag 数值大于所有有效id
			{
				// 注意 id是越小优先级越高的
				maxPriorityTerminalId = thisTerminalId;
			}
		}
        return maxPriorityTerminalId;
    };

    // 设置第一个closure节点
    sMoveType::iterator firstIter;
    bool success;
    closure firstClosure;

	for(const nfa& nfaInVec : inputNfaVec)
	{
		auto closureToInsert = getClosure(nfaInVec.pBegin);
		// 这里不用使用 getClosureInCache, 那样只能增加未击中次数
		assert(nfaInVec.pBegin);
		firstClosure.insert(closureToInsert.begin(), closureToInsert.end());
	}
    std::tie(firstIter, success) = sMove.insert(
        sMoveType::value_type(std::move(firstClosure), sMoveContentType()));
    std::pair<dfaState, int>& firstContent = (*firstIter).second;
    firstContent.first.terminalId = getTerminalId((*firstIter).first);
    firstContent.second = getId();
    
    assert(success);
    std::queue<sMoveType::iterator> toTraverse;
    toTraverse.push(firstIter);
    while(!toTraverse.empty())
    {
        sMoveType::iterator fromIter = toTraverse.front();
        toTraverse.pop();
        const closure& fromClosure = fromIter->first;
        std::pair<dfaState, int>& fromContent = fromIter->second;
        dfaState& fromDfaState = fromContent.first;
        int& fromId = fromContent.second;
        // fromId = getId(); // 这里获得ID是错误的，要在给from.dfaState.next[c]赋值前对id赋值
		closure toClosure;
		// 将closure挪出循环体外，每次循环结束时clear,将运行时间从2.559->2.405
        for(unsigned int c = 0; c <= 126; ++c) // 127是char最大值  是不可打印的char
        {
			// 循环内不能出现 break和continue, 如果需要增加，反注释下面一行并删除scope最后一行的toClosure.clear();
			//cc::onScopeEnd clearToClosure([&toClosure]{toClosure.clear();});
            for(nfaState* pState : fromClosure)
            {
                if(!pState->nextStates.empty())
                {
                    nfaState* next = pState->nextStates[c];
                    // 当 nfaState到终止节点时， next.nextStates 是空的，这里要注意这种情况
                    if(next)
                    {
                        const closure& clos = getClosureInCache(pState->nextStates[c]);
                        // 查找这些节点对应的闭包

                        toClosure.insert(clos.begin(), clos.end());  /////////////28% time
                    }
                }
            }
            // 现在 toClosure 就是 s_move(fromClosure, c）的结果了if(toClosure.size())


            if(toClosure.empty())
			{
                //指向 null, 虽然构造函数中已经初始化为nextDfaIsNullFlag了，但这么做对性能影响几乎没有
                fromDfaState.next[c] = nextDfaIsNullFlag;
            }
            else
            {
		// times 6700

				auto toClosureIter = sMove.find(toClosure); /////////////28% time
				// find(..) != sMove.end() 比sMove.count(..)还要快很多
				if(toClosureIter == sMove.end())
                // 把结果闭包注册到sMove中 
                {

                    auto insertResult = sMove.insert(sMoveType::value_type(toClosure, sMoveContentType()));//// 7% time
                    //同时设置id, id必须这里设置， 因为下面就要对fromDfaState.next[c]赋值了
					toClosureIter = insertResult.first;
                    std::pair<dfaState, int>& content = (*toClosureIter).second;
                    content.second = getId();
                    assert(insertResult.second);
                    toTraverse.push(insertResult.first);
                }
                // 填写 dfaState.next

                assert(toClosureIter != sMove.end());
                std::pair<dfaState, int>& toClosureContent = (*toClosureIter).second;

                fromDfaState.next[c] = toClosureContent.second;
                
                toClosureContent.first.terminalId = getTerminalId(toClosure); //  10%
            }
			toClosure.clear();
        }

    } // SMOVE 深度优先遍历 while循环

    auto reduceToDfaContainer = [&getId]
    (sMoveType& inputSMove) -> std::vector<dfaState>
    {
        assert(inputSMove.size() == getId());
        std::vector<dfaState> result(inputSMove.size());
        for(sMoveType::value_type& value : inputSMove)
        {
            std::pair<dfaState, int> dfaPair = value.second;
            result[dfaPair.second] = std::move(dfaPair.first);
        }
        return result;
    };

    return reduceToDfaContainer(sMove);
}

std::vector<dfaState> getDfa(const nfa inputNfa)
{
    std::vector<nfa> inputNfaVec(1, inputNfa); 
    return getDfa(inputNfaVec);
}



typedef std::pair<std::string::const_iterator, std::string::const_iterator> rangeOfConstStrType;

struct resultOfTokenizerMatchType
{
    rangeOfConstStrType strRange;
    int terminalId;
};

resultOfTokenizerMatchType tokenizerMatch(const std::vector<dfaState>& inputDfa, rangeOfConstStrType src)
{
    typedef resultOfTokenizerMatchType result_type;

    result_type result;
    result.strRange.first  = src.first;
    result.strRange.second = src.first;
    result.terminalId      = nonterminalFlagOfDfaState;
    if(src.first >= src.second)
    {
        return result;
    }

    auto getTerminalId = [&]
    (int inputIndex) -> int
    {
        assert(inputIndex != nextDfaIsNullFlag);
        return inputDfa[inputIndex].terminalId;
    };
    
    auto strIter  = src.first;
    auto strBegin = src.first;
    auto strEnd   = src.second;
    int lastDfaIndex = 0;
    int thisDfaIndex  = 0;

    // 注意index == nextDfaIsNullFlag 和 indexTerminal == nonterminalFlagOfDfaState 意义的不同
    for(; strIter != strEnd; ++strIter)
    {
        assert((thisDfaIndex >= 0) && (unsigned int)thisDfaIndex < inputDfa.size());
        lastDfaIndex = thisDfaIndex;
        thisDfaIndex = inputDfa[thisDfaIndex].next[static_cast<unsigned int>(*strIter)];
        if(thisDfaIndex == nextDfaIsNullFlag)
        {
            if(getTerminalId(lastDfaIndex) != nonterminalFlagOfDfaState)
            {
                result.strRange.second = strIter;
                //strIter 是该匹配串的终止节点
                result.terminalId = getTerminalId(lastDfaIndex);
            }
            return result;
        }
        if(getTerminalId(lastDfaIndex) != nonterminalFlagOfDfaState && getTerminalId(thisDfaIndex) == nonterminalFlagOfDfaState)
        {
            result.strRange.second = strIter;
            //strIter 是该匹配串的终止节点
            result.terminalId = getTerminalId(lastDfaIndex);
            return result;
        }
    }
    // 这里因为lastDfaIndex 没有更新， 所以是判断terminalId of thisDfaIndex
    if(getTerminalId(thisDfaIndex) != nonterminalFlagOfDfaState)
    {
        result.strRange.second = strIter;
        result.terminalId = getTerminalId(thisDfaIndex);
    }
    return result;
}

} // namespace ccDetail


/******************************************************************/
/*                      api                                       */
/******************************************************************/

// muitiMatch 优先级 ： 先长度优先， 再按优先级优先
//  ifa -> tag : id
//  if  -> tag : if
namespace ccDetail
{

std::vector<dfaState> getDfaByStrVec(const std::vector<std::string>& strVec)
{
    cc::pool<cc::pool<nfaState>> poolPool;
    std::vector<nfa> nfaVec;
    for(const std::string& str: strVec)
    { 
        nfa regexNfa;
        cc::pool<nfaState>& nfaPool = *(poolPool.get());
        std::tie(regexNfa, nfaPool) = getNfa(str);
        nfaVec.push_back(regexNfa);
    }
    // nfa的状态在getDfa中也要被用到，所以中途产生的nfaPool 要被poolPool保存
    return getDfa(nfaVec);
}


const lexerIterator& lexerIterator::operator++() // 前置++
{
    resultOfTokenizerMatchType result;
	result = tokenizerMatch(dfaVecReference, 
	strRangeType(rangeOfMatchedContent.second, toMatch.end()));
    rangeOfMatchedContent = result.strRange;
    terminalId = result.terminalId;
    return *this;
}

std::pair<std::string, int> lexerIterator::operator*()
{
	return std::make_pair(std::string(rangeOfMatchedContent.first, rangeOfMatchedContent.second), int(terminalId));
}




	
lexer::iterator lexer::getIterator(const std::string* strPtrToMatch) const
{

	assert(strPtrToMatch);
	if(!strPtrToMatch)
	{
		throw std::exception("invalid pointer!");
	}
    // 使用指针是为了防止 传入临时变量最终导致 strIterator 无效
    iterator result = lexerIterator(*strPtrToMatch, dfaVec);
    ++result;
    return result;
    }

	
void lexer::dfaInit()
{
	try
	{
		dfaVec = getDfaByStrVec(strVec);
	}
	catch(const regex_exception& e)
	{
		throw(std::exception(e.what()));
	}
}



int lexer::regist(std::string&& inputRegexStr)
{
    if(registered)
    {
        throw std::exception("lexServer has already been registered!");
    }
    strVec.push_back(inputRegexStr);
    return nextId++;
}

int lexer::regist(const std::string& inputRegexStr)
{
    if(registered)
    {
        throw std::exception("lexServer has already been registered!");
    }
    strVec.push_back(inputRegexStr);
    return nextId++;
}

void lexer::endRegist()
{
    if(registered)
    {
        throw std::exception("lexServer has already been registered!");
    }
    registered = true;
    dfaInit();
}



} // namespace ccDetail


