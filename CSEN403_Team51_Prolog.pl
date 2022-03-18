									%~~~~~~~~~~~~~~~~~~~~~~~~%% Appendix %%~~~~~~~~~~~~~~~~~~~~~~~~~~~%
%Tag--> tag(StringTag) , String of the binary nb of the tag part.
%Data --> data(MemData), 
%ValidBit--> 0 (trash) , ValidBit-->1 (Valid).
%Order , decimal number ( order of replacement in the cache).
%item(Tag,Data,ValidBit,Order).
									%~~~~~~~~~~~~~~~~~~~~~~~%% Predefined PART %%~~~~~~~~~~~~~~~~~~~~~~~~%
%PREDEFINED:
%atom_number(Atom,Number)--> Integer parseInt
%string_concat(String1, String2, String3).
%---------------------------------------------------------
pow(_,0,1).

pow(X,Y,Z) :- 
					Y>0,
					Y1 is Y - 1,
					pow(X,Y1,Z1), Z is Z1*X.
%---------------------------------------------------------
addzeros(X,BitsNum,N):-
					Z is 6 - BitsNum,
					atom_length(X,L),
					NbOfZeros is Z - L,
					fillZeros(X,NbOfZeros,N).
%---------------------------------------------------------
%Tag+Idx = Adress From memory
%Idx = Address Cache
getfroMem([H|T],0,H).

getfroMem([H|T],Nb,Res):-
					Nb>0,
					Nb1 is Nb-1,
					getfroMem(T,Nb1,Res).			
%---------------------------------------------------------							
nbofDigits(0,1).

nbofDigits(1,1).

nbofDigits(X,N):-
					X>0,
					X1 is X//10,
					nbofDigits(X1,N1).
%---------------------------------------------------------
incValidAcc([],Acc,Acc).																	  %Increments All VALID BITS		

incValidAcc([item(tag(Ta),data(D),1,Obit)|T],Acc,Res):-
					O1 is Obit+1,
					append(Acc,[item(tag(Ta),data(D),1,O1)],NewAcc),
					incValidAcc(T,NewAcc,Res).
					
incValidAcc([item(tag(Ta),data(D),0,Obit)|T],Acc,Res):-
					append(Acc,[item(tag(Ta),data(D),0,Obit)],NewAcc),
					incValidAcc(T,NewAcc,Res).
%---------------------------------------------------------
inValidRep([item(tag(Ta),data(D),0,Obit)|T],TagStr,ItemMem,[item(tag(TagStr),data(ItemMem),1,0)|T]).   %Replaces In the first INVALID ITEM

inValidRep([item(tag(Ta),data(D),1,Obit)|T],TagStr,ItemMem,[item(tag(Ta),data(D),1,Obit)|T2]):-
					inValidRep(T,TagStr,ItemMem,T2).
%---------------------------------------------------------						
highestOrder([],Acc,Acc).				

highestOrder([item(_,_,1,X)|T],Acc,H):- 																%Highest OrderBit If Valid Items
					X>=Acc,
					Acc1=X,
					highestOrder(T,Acc1,H).
					
highestOrder([item(_,_,1,X)|T],Acc,H):-
					X<Acc,
					highestOrder(T,Acc,H).
					
highestOrder([item(_,_,0,X)|T],Acc,H):-
					highestOrder(T,Acc,H).
%---------------------------------------------------------			
getIndex([item(_,_,_,H)|T],H,Acc,Acc).																%Gets the Index of the highest Order

getIndex([item(_,_,_,X)|T],H,Acc,Index):-
					X\=H,
					Acc1 is Acc+1,
					getIndex(T,H,Acc1,Index).
%-----------------------------------------------------------------------------------------------------------------------------------------
									%~~~~~~~~~~~~~~~~~~~~~~~%% COMMON PARTS IN ALL METHODS %%~~~~~~~~~~~~~~~~~~~~~~~~%

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
					getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
					NewCache = OldCache.
										
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
					\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
					atom_number(StringAddress,Address),
					convertAddress(Address,BitsNum,Tag,Idx,Type),
					replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

%---------------------------------------------------------


runProgram([],OldCache,_,OldCache,[],[],Type,_).

runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
					getNumBits(NumOfSets,Type,OldCache,BitsNum),
					(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
					getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
					runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).

									%~~~~~~~~~~~~~~~~~~~~~~~%% GENERAL PART %%~~~~~~~~~~~~~~~~~~~~~~~~%
%-----------------------------------------------------------------------------------------------------------------------------------------


convHelp(0,_,0).

convHelp(Bin,Count,Dec):-
					Bin>0,
					X is Bin mod 10,
					Y is Bin // 10,
					pow(2,Count,Z),
					Temp is Z*X,
					Count1 is Count+1,
					convHelp(Y,Count1,Dec1),
					Dec is Dec1+ Temp.
					
convertBinToDec(Bin,Dec):- 
					convHelp(Bin,0,Dec).
%---------------------------------------------------------
					
replaceIthItem(Item,[H|T],0,[Item|T]).

replaceIthItem(Item,[H|T],I,[H|T1]):-
					I>0,
					I1 is I-1,
					replaceIthItem(Item,T,I1,T1).
%---------------------------------------------------------
splitEvery(N,List,Res):-	
					splithelper(N,N,List,[],[],Res).
					
splithelper(N,0,List,Acc1,Acc2,Res):-
					append(Acc2,[Acc1],Acc2new),
					splithelper(N,N,List,[],Acc2new,Res).

splithelper(_,_,[],[],Acc,Acc).

splithelper(N,K,[H|T],Acc1,Acc2,Res):-	
					K>0,
					K1 is K-1,
					append(Acc1,[H],Acc1new),
					splithelper(N,K1,T,Acc1new,Acc2,Res).
					
removeN(0,T,T).		

removeN(N,[H|T],Res):-
					N1 is N-1,
					removeN(N1,T,Res).					

%---------------------------------------------------------

logBase2(0,0).

logBase2(1,0).

logBase2(Num,Res):-	
					Num>1,
					Num1 is Num//2,
					logBase2(Num1,Res1),
					Res is Res1+1.
%---------------------------------------------------------
				
			
getNumBits(_,fullyAssoc,Cache,0).

getNumBits(NumOfSets,setAssoc,Cache,BitsNum):-
					logBase2(NumOfSets,BitsNum).

getNumBits(_,directMap,Cache,BitsNum):-
					length(Cache,X),
					logBase2(X,BitsNum).
%---------------------------------------------------------
						
fillZeros(X,0,X).

fillZeros(String,N,R):-
					N>0,
					N1 is N-1,
					string_concat("0",String,Res),
					fillZeros(Res,N1,R).
%-----------------------------------------------------------------------------------------------------------------------------------------

							%~~~~~~~~~~~~~~~~~~~~~~~%% Direct Mapping PART %%~~~~~~~~~~~~~~~~~~~~~~~~%
%-----------------------------------------------------------------------------------------------------------------------------------------
	

%%RECONSIDER WITH A NEW APPROACH AND TRY AND USE 6 BITS%%

getDataFromCache(StringAddress,Cache,Data,0,directMap,BitsNum):-
					atom_number(StringAddress,AddNb),
					convertAddress(AddNb,BitsNum,Tag,Idx,directMap),
					atom_number(TagStr,Tag),
					Z is 6 - BitsNum,
					atom_length(TagStr, L),
					NbOfZeros is Z - L,
					fillZeros(TagStr, NbOfZeros, TagStr1),
					convertBinToDec(Idx,Dec),
					getfroMem(Cache,Dec,X),	
					X=item(tag(TagStr1),data(Data),1,_).
															
%---------------------------------------------------------
	
							
helperConvAdd(Nb,Res):-
					pow(10,Nb,Res).
					
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
					helperConvAdd(BitsNum,Res),
					Idx is Bin mod Res,
					Tag is Bin // Res.

%---------------------------------------------------------

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
					atom_number(IdxStr,Idx),
					atom_number(TagStr,Tag),
					atom_length(IdxStr,LI),
					NbOfZ is BitsNum-LI,
					Z is 6 - BitsNum,
					atom_length(TagStr, L),
					NbOfZeros is Z - L,
					fillZeros(IdxStr, NbOfZ, IdxStr1),		
					fillZeros(TagStr, NbOfZeros, TagStr1),
					string_concat(TagStr1,IdxStr1,AddStr),
					atom_number(AddStr,Add),
					convertBinToDec(Add,AddDec),
					convertBinToDec(Idx,IdxDec),
					getfroMem(Mem,AddDec,ItemData), 
					replaceIthItem(item(tag(TagStr1),data(ItemData),1,0),OldCache,IdxDec,NewCache).
					
%-----------------------------------------------------------------------------------------------------------------------------------------

							%~~~~~~~~~~~~~~~~~~~~~~~%% Fully Associative Mapping PART %%~~~~~~~~~~~~~~~~~~~~~~~~%
%-----------------------------------------------------------------------------------------------------------------------------------------

getDataFromCache(StringAddress,[item(tag(StringAddress),data(Data),1,_)|T],Data,0,fullyAssoc,_).
											
getDataFromCache(StringAddress,[item(tag(StringAddress),data(Data),0,_)|T],Data,HopsNum,fullyAssoc,_):-
					getDataFromCache(StringAddress,T,D,H1,fullyAssoc,_),
					HopsNum is H1+1.

getDataFromCache(StringAddress,[item(tag(Tag),data(Data),_,_)|T],D,HopsNum,fullyAssoc,_):-
					StringAddress\=Tag,
					getDataFromCache(StringAddress,T,D,H1,fullyAssoc,_),
					HopsNum is H1+1.
%-----------------------------------------------------------------------------------------------------------------------------------------

convertAddress(Bin,_,Tag,_,fullyAssoc):-
					Tag=Bin.

convertAddress(Bin,_,Bin,_,fullyAssoc):-
					false.
%---------------------------------------------------------

replaceInCache(Tag,Idx,Mem,[item(tag(Ta),data(D),N,Obit)|T],NewCache,ItemData,fullyAssoc,_):-
					convertBinToDec(Tag,TagDec),
					getfroMem(Mem,TagDec,ItemData),
					atom_number(TagStr,Tag),
					atom_length(TagStr,L),
					Z is 6-L,
					fillZeros(TagStr,Z,TagStr1),
					incValidAcc([item(tag(Ta),data(D),N,Obit)|T],[],OldCache1),										
					\+inValidRep(OldCache1,TagStr,ItemData,NewCache),
					highestOrder(OldCache1,0,HighestOrder),
					getIndex(OldCache1,HighestOrder,0,Index),
					replaceIthItem(item(tag(TagStr1),data(ItemData),1,0),OldCache1,Index,NewCache).

replaceInCache(Tag,Idx,Mem,[item(tag(Ta),data(D),N,Obit)|T],NewCache,ItemData,fullyAssoc,_):-
					convertBinToDec(Tag,TagDec),
					getfroMem(Mem,TagDec,ItemData),
					atom_number(TagStr,Tag),
					atom_length(TagStr,L),
					Z is 6-L,
					fillZeros(TagStr,Z,TagStr1),
					incValidAcc([item(tag(Ta),data(D),N,Obit)|T],[],OldCache1), 
					inValidRep(OldCache1,TagStr1,ItemData,NewCache).
					
%-----------------------------------------------------------------------------------------------------------------------------------------

							%~~~~~~~~~~~~~~~~~~~~~~~%% Set Associative Mapping PART %%~~~~~~~~~~~~~~~~~~~~~~~~%
%-----------------------------------------------------------------------------------------------------------------------------------------

splitString(String,NumBits,TagFinal,Index):-
					atom_number(String,StringNumb),
					nbofDigits(StringNumb,SND),   
					pow(10,NumBits,POW1),
					Index is StringNumb mod POW1,
					Tag is StringNumb // POW1,
					atom_number(TagString,Tag),
					atom_length(String,SLength),
					atom_length(TagString,TLength),
					Diff is SLength-NumBits,
					Z is Diff-TLength,
					fillZeros(TagString,Z,TagFinal),
					!.
							
%-----------------------------------------------------------------------------------------------------------------------------------------

getDataSetAssoc(Tag,[item(tag(AtomTag),data(Data),1,_)|T],Data,0,_):-
					atom_number(AtomTag,TagBin),
					TagBin = Tag.
					
getDataSetAssoc(Tag,[item(_,_,0,_)|T],Data,HopsNum,_):-
					getDataSetAssoc(Tag,T,Data,H1,_),
					HopsNum is H1+1.

getDataSetAssoc(Tag,[item(tag(AtomTag),_,_,_)|T],Data,HopsNum,_):-
					TagBin\=Tag,
					atom_number(AtomTag,TagBin),
					getDataSetAssoc(Tag,T,Data,H1,_),
					HopsNum is H1+1.

getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
					length(Cache,CacheLength),
					SplitIndex is CacheLength//SetsNum,
					splitEvery(SplitIndex,Cache,CacheSets),
					getNumBits(SetsNum,setAssoc,CacheSets,BitsNum),
					atom_number(StringAddress,StringNumb),
					convertAddress(StringNumb,SetsNum,Tag,Index,setAssoc),
					convertBinToDec(Index,IndexDec),
					getfroMem(CacheSets,IndexDec,WantedSet),
					getDataSetAssoc(Tag,WantedSet,Data,HopsNum,SetsNum).
%-----------------------------------------------------------------------------------------------------------------------------------------

convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
					logBase2(SetsNum,NbBits),
					Z is 10^NbBits,
					Idx is Bin mod Z,
					Tag is Bin // Z.
																			
%-----------------------------------------------------------------------------------------------------------------------------------------
					

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
					length(OldCache,CacheLength),
					SplitIndex is CacheLength//SetsNum,
					atom_number(TagStr,Tag),
					atom_number(IdxStrr,Idx),
					string_concat(TagStr,IdxStrr,Address),
					atom_number(Address,AddBin),
					convertBinToDec(AddBin,AddDec),
					splitEvery(SplitIndex,OldCache,CacheSets),
					convertBinToDec(Idx,IdxDec),
					getfroMem(CacheSets,IdxDec,WantedSet),
					convertBinToDec(Tag,TagDec),
					getfroMem(Mem,AddDec,ItemData),
					atom_number(TagStr,Tag),
					atom_length(TagStr,L),
					logBase2(SetsNum,X),
					Ress is 6-X,
					Z is Ress-L,
					fillZeros(TagStr,Z,TagStr1),
					incValidAcc(WantedSet,[],OldCache1),										
					\+inValidRep(OldCache1,TagStr,ItemData,NewCache1),
					highestOrder(OldCache1,0,HighestOrder),
					getIndex(OldCache1,HighestOrder,0,Index),
					replaceIthItem(item(tag(TagStr1),data(ItemData),1,0),OldCache1,Index,NewCache1),
					replaceIthItem(NewCache1,CacheSets,IdxDec,CacheSetsNew),
					flatten(CacheSetsNew,NewCache).
											
											
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
					length(OldCache,CacheLength),
					SplitIndex is CacheLength//SetsNum,
					atom_number(TagStr,Tag),
					atom_number(IdxStrr,Idx),
					string_concat(TagStr,IdxStrr,Address),
					atom_number(Address,AddBin),
					convertBinToDec(AddBin,AddDec),
					splitEvery(SplitIndex,OldCache,CacheSets),
					convertBinToDec(Idx,IdxDec),
					getfroMem(CacheSets,IdxDec,WantedSet),
					convertBinToDec(Tag,TagDec),
					getfroMem(Mem,AddDec,ItemData),
					%atom_length(ItemData,MaxL),
					atom_number(TagStr,Tag),
					atom_length(TagStr,L),
					logBase2(SetsNum,X),
					Ress is 6-X,
					Z is Ress-L,
					fillZeros(TagStr,Z,TagStr1),
					incValidAcc(WantedSet,[],OldCache1), 
					inValidRep(OldCache1,TagStr1,ItemData,NewCache1),
					replaceIthItem(NewCache1,CacheSets,IdxDec,CacheSetsNew),
					flatten(CacheSetsNew,NewCache).
