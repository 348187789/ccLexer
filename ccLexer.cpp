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
        first = std::move(other.first); //
    }

    ~pool(){}

    void operator=(pool&& other)
    {
        last = std::move(other.last);
        first = std::move(other.first);
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
        ); // ����Ĭ���ǰ�ֵ����ģ����Ҫ�����ô�ֵ����Ҫʹ��std::ref();
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
    char rangeEnd; // range(a, b) contains char 'b'

}; // class charRange

class numRange // TODO with "x{0-9}"" 
{
};



// nfa �ڵ㱣֤ �׽ڵ㲻�������ڵ�ָ�� β�ڵ㲻ָ�� �����ڵ�
// ��������nfa ��ֻ�裬swap(a.end, b.first)
// ��a.first �� b.end ��� ��nfa
// a.end������


struct nfaState
{
    std::vector<nfaState*> nextStates;
    std::vector<nfaState*> epsiStates;

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
        if(nextStates.empty())
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
    /*       nfa����       */
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
    /*       regex��ȡ     */
    /***********************/

    auto readCharRange = [&]
    () -> charRange 
    {
        if(isSpecialChar(*strIter))
		{
            return charRange('\0'); // ����ʱ�ᱻ��� *strIter == ']'
		}
        char first = *strIter++;
        regex_assert(*strIter == '-', "valid char range");
        ++strIter;
        regex_assert(*strIter != char(0)  , "valid char range");
        char end = *strIter;
        ++strIter;
        // ����ʱ strIter
        return charRange(first, end);

    }; // regex::readCharRange()


    // ��ΪҪ�ݹ飬Ҫд����������,���Ҳ���ʹ��auto
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
                // | ���ȼ���ͣ� ������ǰ�����е�nfaState
                // ��������*+����������ǰһ���ַ���
                storePre();
                ++strIter;
                nfa orNfa = readNfa();
                // ע��˴�������readNfa, 
                // strIter�Ѿ�ָ������һ��Ҫ�����ַ�
                // ��readNfa�ڵ�������(readNfa��һ��ֻ��forѭ����)
                // ����ÿ�ζ�Ҫ--strIter������for���++strIter
                // ʹ���ַ��ܱ���ȷ����
                --strIter;

                assert(orNfa);
                preNfa = orLink(preNfa, orNfa); 
                // Ϊ�˱�������״̬(*+����������������thisNfa;
                // ��״̬�ָ���thisNfa
                thisNfa = preNfa; 
                preNfa.pBegin = nullptr;
                preNfa.pEnd   = nullptr;
            }
            else if(equal(*strIter, '.'))
            {
                storePre();
                thisNfa = constructByCharRange( charRange(char(1), char(126)) ); 
                                            // 127��char�����ֵ���ᵼ��range��ֵ����ѭ��
            }
            else if(equal(*strIter, '*'))
            {   // TODO : ����ĸ�Ϊ repeat(sd, numRange(0, 1))��
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
                assert(thisNfa); //readNfa �ڷ���ʱȷ�����ص�����Чsd�� 
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
                ++strIter; // ÿһ��readXX()����ֱ�Ӵ�*strIter����
                           // �������ں������غ�Ҳ��ֱ���ж�*strIter�ǲ���ָ���ս��
                           // readXX()�л�Ҫ�������'\0'
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
				// TODO : x{1-9}
                //numRange nr = readNumRange();
                //regex_assert(equal(*strIter, '}'), "unmatched }\n");
                //thisNfa = repeat(thisNfa, nr);
                regex_assert(strIter!= strEnd && equal(*strIter, '}'), "unmatched '{'\n");
            }
            else if(equal(*strIter, '}'))
            {
                break;
            }
            else if(equal(*strIter, '/'))
            {   // TODO ���Ӹ����ת���ַ� ��/d
                storePre();
                strIter++;
                regex_assert(isSpecialChar(*strIter), " illegal escape character"); //�Ƿ���ת���ַ�
                char special = *strIter;
                thisNfa = constructByCharRange(charRange(special));
            }
            else if(equal(*strIter, '\\'))
            {
                storePre();
                strIter++;
                regex_assert(isSpecialChar(*strIter), " illegal escape character"); //�Ƿ���ת���ַ�
                char special = *strIter;
                thisNfa = constructByCharRange(charRange(special));
            }
        }
        storePre();
        if(!preNfa)
            regex_assert(false, "empty regex\n");
        return preNfa;
    }; // regex::readNfa()

	nfa result;
    try{
		result = readNfa();
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





typedef std::set<nfaState*> closure;
// ����ʹ��unordered_set��assertin fail, ��ʱ��û����ԭ��

closure getClosure(nfaState* inputNfaState)
{
	// ʵ�ʴ�����ʹ�õ���ʹ���ǰ�װ���getClosureInCache
    closure result;
    assert(inputNfaState);
    if(nullptr == inputNfaState)
    {
        return result; 
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


// ÿ�β�ѯ�ıհ������dfaʹ�õ��ıհ�����ͬһ�����ϣ�
// ���в�ѯ���ĵ��ڵ��Ӧ�ıհ������ closureChache����
//  closureCache type : std::unordered_map<nfaState*, closure>
//  getClosureCache type : function<const closure&(nfaState*)>

// �����γ� dfa �ıհ������ sMove ��
// sMoveType : std::map< closure, std::pair<dfaState, int> >
// ���� int ���������� dfa��ID

class hashOfClosure
{
public:
	std::size_t operator()(const closure& inputClosure) const 
	{
		size_t hashValue = 0;
		for(nfaState* pState : inputClosure)
		{
			hashValue += unsigned int(pState) + unsigned int(pState) & 31 * 33 ;
		}
		return hashValue;
	};

};

//

std::vector<dfaState> getDfa(const std::vector<nfa>& inputNfaVec)
{
    // inputVec �е��г������ȼ��Ӹߵ��͵�regex��Ӧ��nfa����СΪ1ʱ��ֱ������regex_match
    // ��ʱʹ�����ַ�������dfa״̬��������������Ӱ�죬 to�Ľ�
    
    assert(inputNfaVec.size() > 0);

    /**************************************************/
    /*             lamda����                            */
    /**************************************************/
	std::unordered_map<nfaState*, closure> closureCache;
    typedef decltype(closureCache) closureCacheType;

    auto getClosureInCache = [closureCache] // ����ʹ��mutable��cache��Ϊfunction��Ա�������ⲿ�ı�
    (nfaState* inputNfaState) mutable -> const closure&
    {
        closureCacheType::iterator resultIterator = closureCache.find(inputNfaState);
        if(resultIterator != closureCache.end()) 
        // ���map����statePtr��Ϣ �� ֱ�Ӳ�ѯ����
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
    

    // ��������

    std::unordered_map<closure, std::pair<dfaState, int>, hashOfClosure> sMove;
	sMove.max_load_factor(2);//��1->2��������������ʱ�� 2.3->2.0
    typedef decltype(sMove) sMoveType;
    typedef std::pair<dfaState, int> sMoveContentType;


	// getTerminalId ʹ�����㷨 �ڸú����������40% ��ʱ�併��8%
	std::unordered_map<nfaState*, int> terminalIdMap;
	typedef decltype(terminalIdMap) terminalIdMapType;

	auto terminalIdOfNfaStateMaker = 0;
	for(const nfa& n : inputNfaVec)
	{
		terminalIdMap.insert(terminalIdMapType::value_type(n.pEnd, terminalIdOfNfaStateMaker));
		++terminalIdOfNfaStateMaker;
	}

	auto getTerminalId = [&inputNfaVec, &terminalIdMap]
    //  idΪ nonterminalFlagInTerminalId ���ʾ�ýڵ㲻���ս�ڵ㣬 ������id = iʱ���ʾ�ýڵ���������ȼ�Ϊ i ���ս�ڵ�
    (const closure& inputClosure) -> int
    {
		assert(nonterminalFlagOfDfaState > 1000000); // �㷨��Ҫflag������������id
		int maxPriorityTerminalId = nonterminalFlagOfDfaState;
		for(nfaState* inputNfaStatePtr : inputClosure)
		{
			int thisTerminalId = nonterminalFlagOfDfaState;
			auto findIter = terminalIdMap.find(inputNfaStatePtr); 
			if(findIter != terminalIdMap.end())
			{
				thisTerminalId = (*findIter).second;
			}
			if(thisTerminalId < maxPriorityTerminalId) // nonterminalFlag ��ֵ����������Чid
			{
				// ע�� id��ԽС���ȼ�Խ�ߵ�
				maxPriorityTerminalId = thisTerminalId;
			}
		}
        return maxPriorityTerminalId;
    };

    // ���õ�һ��closure�ڵ�
    sMoveType::iterator firstIter;
    bool success;
    closure firstClosure;

	for(const nfa& nfaInVec : inputNfaVec)
	{
		auto closureToInsert = getClosure(nfaInVec.pBegin);
		// ���ﲻ��ʹ�� getClosureInCache, ����ֻ������δ���д���
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
        // fromId = getId(); // ������ID�Ǵ���ģ�Ҫ�ڸ�from.dfaState.next[c]��ֵǰ��id��ֵ
		closure toClosure;
		// toClosure��Ų��ѭ�������������ܻ��Ƚ�����
        for(unsigned int c = 0; c <= 126; ++c) // 127��char���ֵ  �ǲ��ɴ�ӡ��char
        {
			//������ ѭ���ڲ��ܳ��� break��continue, �����Ҫ���ӣ���ע������һ�в�ɾ��scope���һ�е�toClosure.clear();
			//cc::onScopeEnd clearToClosure([&toClosure]{toClosure.clear();});
            for(nfaState* pState : fromClosure)
            {
                if(!pState->nextStates.empty())
                {
                    nfaState* next = pState->nextStates[c];
                    // next����Ϊ��
                    if(next)
                    {
                        const closure& clos = getClosureInCache(pState->nextStates[c]);
                        // ������Щ�ڵ��Ӧ�ıհ�

                        toClosure.insert(clos.begin(), clos.end());  /////////////28% time
                    }
                }
            }

            // ���� toClosure ���� s_move(fromClosure, c���Ľ����if(toClosure.size())
            if(toClosure.empty())
			{
                //ָ�� null, ��Ȼ���캯�����Ѿ���ʼ��ΪnextDfaIsNullFlag�ˣ�����ô��������Ӱ�켸��û��
                fromDfaState.next[c] = nextDfaIsNullFlag;
            }
            else
            {
		// times 6700

				auto toClosureIter = sMove.find(toClosure); /////////////28% time
				if(toClosureIter == sMove.end())
                {
					// �����д��sMove

                    auto insertResult = sMove.insert(sMoveType::value_type(toClosure, sMoveContentType()));//// 7% time
                    // id�����������ã� ��Ϊ�����Ҫ��fromDfaState.next[c]��ֵ��
					toClosureIter = insertResult.first;
                    std::pair<dfaState, int>& content = (*toClosureIter).second;
                    content.second = getId();
                    assert(insertResult.second);
                    toTraverse.push(insertResult.first);
                }
                // ��д dfaState.next

                assert(toClosureIter != sMove.end());
                std::pair<dfaState, int>& toClosureContent = (*toClosureIter).second;

                fromDfaState.next[c] = toClosureContent.second;
                
                toClosureContent.first.terminalId = getTerminalId(toClosure); //  10%
            }
			toClosure.clear();
        }

    } // SMOVE ������ȱ��� whileѭ��

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



typedef std::pair<std::string::const_iterator, std::string::const_iterator> constStrRangeType;

struct matchedContentType
{
	constStrRangeType matchedStrRange;
	int terminalId;
};

matchedContentType tokenizerMatch(const std::vector<dfaState>& inputDfa, constStrRangeType src)
{
    matchedContentType result;
    result.matchedStrRange.first  = src.first; // stringRange.first = src.first
    result.matchedStrRange.second = src.first; // stringRange.second = src.first
    result.terminalId  = nonterminalFlagOfDfaState;
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

    // ע��index == nextDfaIsNullFlag �� indexTerminal == nonterminalFlagOfDfaState ����Ĳ�ͬ
    for(; strIter != strEnd; ++strIter)
    {
        assert((thisDfaIndex >= 0) && (unsigned int)thisDfaIndex < inputDfa.size());
        lastDfaIndex = thisDfaIndex;
        thisDfaIndex = inputDfa[thisDfaIndex].next[static_cast<unsigned int>(*strIter)];
        if(thisDfaIndex == nextDfaIsNullFlag)
        {
            if(getTerminalId(lastDfaIndex) != nonterminalFlagOfDfaState)
            {
                result.matchedStrRange.second = strIter;
                //strIter �Ǹ�ƥ�䴮����ֹ�ڵ�
                result.terminalId = getTerminalId(lastDfaIndex);
            }
            return result;
        }
        if(getTerminalId(lastDfaIndex) != nonterminalFlagOfDfaState && getTerminalId(thisDfaIndex) == nonterminalFlagOfDfaState)
        {
            result.matchedStrRange.second = strIter;
            //strIter �Ǹ�ƥ�䴮����ֹ�ڵ�
            result.terminalId = getTerminalId(lastDfaIndex);
            return result;
        }
    }
    // ������ΪlastDfaIndex û�и��£� �������ж�terminalId of thisDfaIndex
    if(getTerminalId(thisDfaIndex) != nonterminalFlagOfDfaState)
    {
        result.matchedStrRange.second = strIter;
        result.terminalId = getTerminalId(thisDfaIndex);
    }
    return result;
}

} // namespace ccDetail


/******************************************************************/
/*                      api                                       */
/******************************************************************/

// muitiMatch ���ȼ� �� �ȳ������ȣ� �ٰ����ȼ�����
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
    // nfa��״̬��getDfa��ҲҪ���õ���������;������nfaPool Ҫ��poolPool����
    return getDfa(nfaVec);
}


const lexerIterator& lexerIterator::operator++() // ǰ��++
{
    matchedContentType result;
	result = tokenizerMatch(dfaVecReference, 
		strRangeType(rangeOfMatchedContent.second, toMatch.end()));
    rangeOfMatchedContent = result.matchedStrRange;
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
	if(!registered)
	{
		throw std::exception("lexer  has not been registered!");
	}
    // ʹ��ָ����Ϊ�������û���Ҫʹ����ʱ�������յ��� strIterator ��Ч
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


