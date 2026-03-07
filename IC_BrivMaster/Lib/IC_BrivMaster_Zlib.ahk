class IC_BrivMaster_Budget_Zlib_Class ;A class for applying z-lib compatible compression. Badly. This is aimed at strings of <100 characters
{
	__New() ;Pre-computes binary values for various things to improve run-time performance for compression. Note that this is not done the other way as the current intended use is only performance sensitive for compression - so avoid wasting memory for things that may never be used
	{
		BASE64_CHARACTERS:="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" ;RFC 4648 S4, base64
		this.BASE64_TABLE:={} ;Binary key to character
		this.BASE64_REVERSE_TABLE:={} ;Character key to binary string
		Loop, Parse, BASE64_CHARACTERS
		{
			bin:=this.IntToBinaryString(A_Index-1,6)
			this.BASE64_TABLE[bin . ""]:=A_LoopField . "" ;Note: bin must be forced to a string, as whilst it would work as a number as long as processing used the same, the reverse table must have the leading zeros
			this.BASE64_REVERSE_TABLE[ASC(A_LoopField)]:=bin . "" ;This cannot use the character directly as object members are case-insensitive
		}
		this.HOFFMAN_CHARACTER_TABLE:={}
		loop 256
		{
			this.HOFFMAN_CHARACTER_TABLE[A_Index-1]:=this.CodeToBinaryString(A_Index-1)
		}
		this.LENGTH_TABLE:={}
		loop 256
		{
			this.LENGTH_TABLE[A_INDEX+2]:=this.GetLengthCode(A_INDEX+2) ;3 to 258
		}
		this.DISTANCE_TABLE:={}
		loop 64 ;As there are 32K distance values, only pre-calculate the short common ones. Others will be done as needed
		{
			this.DISTANCE_TABLE[A_INDEX]:=this.CalcDistanceCode(A_Index)
		}
	}

	;----------------------------------

	Deflate(inputString,minMatch:=3,maxMatch:=258) ;minMatch must be at least 3, and maxMatch must be at most 258 TODO: This is in dire need of some error handling
	{
		pos:=1
		inputLength:=StrLen(inputString)
		output:="" ;Note: Accumulating the existing output appears to be a tiny bit faster than not doing so and having more complex string operations
		outputBinary:="00011110" . "01011011" . "110" ;2 bytes of header (LSB first), 3 bits of block header
		while(pos<=inputLength)
		{
			if(inputLength-pos+1>=minMatch) ;If there are enough characters left for a minimum match. +1 is there because the character in the current position is included
			{
				match:=1
				distance:=0
				curLookahead:=minMatch
				while(match AND pos+curLookahead-1<=inputLength AND curLookahead<=maxMatch) ;-1 as the current character is included (i.e. SubStr(haystack,startPosition,3) takes 3 characters starting from position 1, so ends at startPosition+2
				{
					lookAhead:=SubStr(inputString,pos,curLookahead)
					match:=inStr(output,lookAhead,1,0) ;Look for an exact match, looking backwards (right to left). MUST be case-sensitive
					if(match AND pos-match<=32768)
					{
						distance:=pos-match
						lastFoundlookAhead:=lookAhead
						matchLength:=curLookahead ;We can use curLookahead instead of StrLen(lookAhead) as we checked in the while clause that there is enough remaining characters to fill the subStr
					}
					else ;Look for repeats, e.g. if the lookahead is abc, see if the previous characters were abc, if aaa, see if previous character was a
					{
						loop % curLookahead-1 ;An exact match would be covered above
						{
							endChunk:=SubStr(output,-A_Index+1) ;Start of 0 means return last character, -2 means return the last 3 characters
							if(this.StringRepeat(endChunk,curLookahead)==lookAhead)
							{
								match:=A_Index
								distance:=match
								matchLength:=curLookahead
								lastFoundlookAhead:=lookAhead
							}
						}
					}
					curLookahead++
				}
				if(distance) ;3+ char string exists in output buffer
				{
					output.=lastFoundlookAhead
					outputBinary.=this.LENGTH_TABLE[matchLength]
					outputBinary.=this.GetDistanceCode(distance)
					pos+=matchLength
					Continue
				}
			}
			char:=SubStr(inputString,pos,1)
			output.=char

			byteValue := NumGet(&inputUTF8, pos, "UChar")   ; 0..255
			code:=ASC(char)
			if(code>255)
				OutputDebug % char . " - " . code . "`n"

			outputBinary.=this.HOFFMAN_CHARACTER_TABLE[ASC(char)]
			pos++
		}
		outputBinary.="0000000" ;End of block, 256-256=0 as 7 bits, which we might as well hard-code
		while(MOD(StrLen(outputBinary),8)) ;Pad to byte boundry
			outputBinary.="0"
		outputBinary:=this.ReverseByteOrder(outputBinary) ;Reverse prior to adding Adler32
		adler32:=this.Adler32(inputString)
		Loop 32
			outputBinary.=((adler32 >> (32-A_Index)) & 1)
		outputBase64:=this.BinaryStringToBase64(outputBinary)
		return outputBase64
	}

	;----------------------------------

	Inflate(inputString)
	{
		bitString:=this.Base64ToBinaryString(inputString)
		BitStream:=new IC_BrivMaster_Budget_Zlib_Class.IC_BrivMaster_Budget_Zlib_Read_Bitstream_Class(bitString)
		CMF_CM:=BitStream.ReadAsLSB(4)
		CMF_CINFO:=BitStream.ReadAsLSB(4) ;Only used for the <=7 check
		FLG_FCHECK:=BitStream.ReadAsLSB(5)
		FLG_FDICT:=BitStream.ReadAsLSB(1)+0 ;Not used here (only valid input is 0) but part of the FCHECK checksum
		FLG_FLEVEL:=BitStream.ReadAsLSB(2) ;FLEVEL is not used in decompression, but is part of the FCHECK checksum
		HEADER:=CMF_CINFO . CMF_CM . FLG_FLEVEL . FLG_FDICT . FLG_FCHECK ;CMF*256 + FLG
		CMF_CM:=this.BinaryStringToInt(CMF_CM)
		CMF_CINFO:=this.BinaryStringToInt(CMF_CINFO)
		if(Mod(this.BinaryStringToInt(HEADER),31)) ;header checksum
		{
			return "" ;Need to throw error here
		}
		if(CMF_CM!=8)
		{
			return "" ;Need to throw error here
		}
		if(CMF_CINFO>7) ;Not used, but check valid per RFC
		{
			return "" ;Need to throw error here
		}
		if(FLG_FDICT!=0)
		{
			return "" ;Need to throw error here
		}
		;Build lookup tables. These are not pre-calculatea as the current use case for this class is performance-sensitive deflate, but insensitive inflate, so doing so would just waste memory
		LIT_LEN_TABLE:={} ;Table of binary strings to code value
		Loop 144 ;0-143	8 bits	codes	00110000 .. 10111111
			LIT_LEN_TABLE[this.IntToBinaryString(0x30 + A_Index-1,8) . ""] := A_Index-1
		Loop 112 ;144-255	9 bits	codes	110010000 .. 111111111
			LIT_LEN_TABLE[this.IntToBinaryString(0x190 + A_Index-1,9) . ""] := 143 + A_Index
		Loop 24 ;256-279	7 bits	codes	0000000 .. 0010111
			LIT_LEN_TABLE[this.IntToBinaryString(A_Index-1,7) . ""] := 256 + A_Index-1
		Loop 8 ;280-287	8 bits	codes	11000000 .. 11000111
			LIT_LEN_TABLE[this.IntToBinaryString(0xC0 + A_Index-1,8) . ""] := 280 + A_Index-1
		LEN_CODE_EXTRA:=[0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0]
		LEN_CODE_TABLE:={} ;TODO: Could this technique be applied to generating the deflate lookups?
		curLength:=3 ;Starts at 3
		loop 29 ;29 values, 257 through 285
		{
			extra:=LEN_CODE_EXTRA[A_Index]
			LEN_CODE_TABLE[A_Index+256,"L"]:=curLength
			LEN_CODE_TABLE[A_Index+256,"E"]:=extra
			curLength+=2 ** extra
		}
		LEN_CODE_TABLE[285,"L"]:=258 ;Exception for 285, as 284 could encode this length but it is given this unique code, presumably because it's expected to occur a lot
		DIST_CODE_EXTRA:=[0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13]
		DIST_CODE_TABLE:={}
		curDist:=1 ;Starts at 1
		Loop 30 ;Codes 0 through 29. 30 and 31 are unused
		{
			extra:=DIST_CODE_EXTRA[A_Index] ;No -1 as the hard-coded list is not 0-indexed
			DIST_CODE_TABLE[A_Index-1,"D"]:=curDist
			DIST_CODE_TABLE[A_Index-1,"E"]:=extra
			curDist+=2 ** extra
		}
		;Process blocks
		BFINAL:=0
		dataOutput:=""
		while(!BFINAL)
			{
			BFINAL:=BitStream.ReadAsLSB(1)+0 ;Convert straight to int since only single bit
			BTYPE:=BitStream.ReadAsLSB(2) ;LSB first to confuse us
			if(BTYPE!="01")
			{
				return "" ;Need to throw error here
			}
			loop
			{
				symbol:=BitStream.ReadAsMSB(7) ;Start with 7
				if(!LIT_LEN_TABLE.HasKey(symbol . ""))
				{
					symbol.=BitStream.ReadAsMSB(1)
					if(!LIT_LEN_TABLE.HasKey(symbol . ""))
					{
						symbol.=BitStream.ReadAsMSB(1)
						if(!LIT_LEN_TABLE.HasKey(symbol . ""))
						{
							return "" ;Need to throw error here
						}
					}
				}
				code:=LIT_LEN_TABLE[symbol . ""]
				if(code<256) ;Literal
					dataOutput.=Chr(code)
				else if(code==256) ;End of block
				{
					break ;Break the inner block loop
				}
				else if(code<=285) ;Length code
				{
					length:=LEN_CODE_TABLE[code].L
					extraBits:=LEN_CODE_TABLE[code].E
					if(extraBits)
					{
						extraBin:=BitStream.ReadAsLSB(extraBits)
						extraDec:=this.BinaryStringToInt(extraBin)
						length+=extraDec
					}
					distanceBinary:=BitStream.ReadAsMSB(5)
					distanceBase:=this.BinaryStringToInt(distanceBinary)
					distance:=DIST_CODE_TABLE[distanceBase].D
					extraBits:=DIST_CODE_TABLE[distanceBase].E
					if(extraBits)
					{
						extraBin:=BitStream.ReadAsLSB(extraBits)
						extraDec:=this.BinaryStringToInt(extraBin)
						distance+=extraDec
					}
					repeat:=SubStr(dataOutput,StrLen(dataOutput)-distance+1,length) ;If L>D this will return less than the requested length, handled below
					if(length>distance) ;If L>D repeat what we have to fill L
						dataOutput.=this.StringRepeat(repeat,length)
					else
						dataOutput.=repeat
				}
				else
				{
					return "" ;Need to throw error here
				}
			}
		}
		;Read to byte boundry
		BitStream.MoveToNextByte()
		ADLER32Bin:=BitStream.ReadAsLSB(8) . BitStream.ReadAsLSB(8) . BitStream.ReadAsLSB(8) . BitStream.ReadAsLSB(8) ;Since we're set up to read the LSB-first bitstream this has to be read byte-by-byte
		ADLER32Int:=this.BinaryStringToInt(ADLER32Bin)
		outputADLER32:=this.ADLER32(dataOutput)
		if(outputADLER32==ADLER32Int)
			return dataOutput
		else
		{
			return "" ;Need to throw error here
		}
	}

	;----------------------------------

	BinaryStringToBase64(string) ;Requires string to have a length that is a multiple of 8
	{
		pos:=1
		stringLength:=StrLen(string)
		while(pos<stringLength)
		{
			slice:=SubStr(string,pos,24) ;Take 24bits at a time
			sliceLen:=StrLen(slice)
			if(sliceLen==24) ;Standard case
			{
				loop, 4
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6) . ""] ;Concatenate with "" to force to string
				pos+=24
			}
			else if (sliceLen==16) ;16 bits, need to pad with 2 zeros to reach 18 and be divisible by 3, then add an = to replace the last 6-set
			{
				slice.="00"
				loop, 3
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6) . ""]
				accu.="="
				Break ;Since we're out of data
			}
			else if (sliceLen==8) ;8 bits, need to pad with 4 zeros to reach 12 and be divisible by 2, then add == to replace the last two 6-sets
			{
				slice.="0000"
				loop, 2
					accu.=this.BASE64_TABLE[SubStr(slice,6*(A_Index-1)+1,6) . ""]
				accu.="=="
				Break ;Since we're out of data
			}
		}
		return accu
	}

	Base64ToBinaryString(base64String)
	{
		pos:=1
		stringLength:=StrLen(base64String)
		if(mod(stringLength,4))
		{
			OutputDebug % "Base64ToBinaryString(): Length [" . stringLength . "] is not a multiple of 4`n"
			return ""
		}
		accu:=""
		while(pos<stringLength)
		{
			slice:=SubStr(base64String,pos,4) ;4 characters at a time, to form 4x6=24bits=3bytes
			chars:=StrSplit(slice)
			if(chars[3]=="=" AND chars[4]=="=") ;Double pad, 1 byte of input + 4 added zeros, so take only the first 2 bits from the 2nd char
			{
				accu.=this.BASE64_REVERSE_TABLE[ASC(chars[1])] . subStr(this.BASE64_REVERSE_TABLE[ASC(chars[2])],1,2)
			}
			else if(chars[4]=="=") ;Single pad, 2 bytes of input + 2 added zeros, so take only the first 4 bits from the 3rd char
			{
				accu.=this.BASE64_REVERSE_TABLE[ASC(chars[1])] . this.BASE64_REVERSE_TABLE[ASC(chars[2])] . subStr(this.BASE64_REVERSE_TABLE[ASC(chars[3])],1,4)
			}
			else
			{
				accu.=this.BASE64_REVERSE_TABLE[ASC(chars[1])] . this.BASE64_REVERSE_TABLE[ASC(chars[2])] . this.BASE64_REVERSE_TABLE[ASC(chars[3])] . this.BASE64_REVERSE_TABLE[ASC(chars[4])]
			}
			pos+=4
		}
		return accu
	}

	StringRepeat(string,length) ;Repeats string until Length is reached, including partial repeats. Eg string=abc length=5 gives abcab
	{
		loop % Ceil(length/StrLen(string))
			output.=string
		return SubStr(output,1,length)
	}

	Adler32(data) ;Per RFC 1950
	{
		s1:=1
		s2:=0
		Loop Parse, data
		{
			byte:=Asc(A_LoopField)
			s1:=Mod(s1 + byte, 65521)
			s2:=Mod(s2 + s1, 65521)
		}
		return (s2 << 16) | s1
	}

	ReverseByteOrder(string) ;We assemble LSB-first as required for the hoffman encoding, but need to be MSB-first for the Base64 conversion. Requires string to have length that is a multiple of 8. Doing the 8 bits explictly seems fractionally faster than using a loop
	{
		pos:=1
		while(pos<StrLen(string))
		{
			accu.=SubStr(string,pos+7,1)
			accu.=SubStr(string,pos+6,1)
			accu.=SubStr(string,pos+5,1)
			accu.=SubStr(string,pos+4,1)
			accu.=SubStr(string,pos+3,1)
			accu.=SubStr(string,pos+2,1)
			accu.=SubStr(string,pos+1,1)
			accu.=SubStr(string,pos,1)
			pos+=8
		}
		return accu
	}

	GetDistanceCode(distance) ;Uses the lookup table for values up to 64, and calls the calculation of higher distances
	{
		if(distance<=64)
			return this.DISTANCE_TABLE[distance]
		else
			return this.CalcDistanceCode(distance)
	}

	CodeToBinaryString(code) ;Takes an ASCII character code, e.g. "97" for "a" and returns the fixed Hoffman-encouded binary representation as a LSB-first string. Used to pre-calculate the lookup table
	{
		if(code>=0 AND code<=143)
		{
			code+=0x30
			bits:=8
		}
		else if(code>=144 AND code<=255)
		{
			code+=0x100
			bits:=9
		}
		else
			MSGBOX % "Invalid character code"
		return this.IntToBinaryString(code,bits)
	}

	GetLengthCode(length) ;Used to pre-calculate the lookup table
	{
		if(length==3) ;Simple cases, no extra bits
			return "0000001"
		else if(length==4)
			return "0000010"
		else if(length==5)
			return "0000011"
		else if(length==6)
			return "0000100"
		else if(length==7)
			return "0000101"
		else if(length==8)
			return "0000110"
		else if(length==9)
			return "0000111"
		else if(length==10)
			return "0001000"
		else if(length<=12)
			return "0001001" . this.IntToBinaryStringLSB(length-11,1)
		else if(length<=14)
			return "0001010" . this.IntToBinaryStringLSB(length-13,1)
		else if(length<=16)
			return "0001011" . this.IntToBinaryStringLSB(length-15,1)
		else if(length<=18)
			return "0001100" . this.IntToBinaryStringLSB(length-17,1)
		else if(length<=22)
			return "0001101" . this.IntToBinaryStringLSB(length-19,2)
		else if(length<=26)
			return "0001110" . this.IntToBinaryStringLSB(length-23,2)
		else if(length<=30)
			return "0001111" . this.IntToBinaryStringLSB(length-27,2)
		else if(length<=34)
			return "0010000" . this.IntToBinaryStringLSB(length-31,2)
		else if(length<=42)
			return "0010001" . this.IntToBinaryStringLSB(length-35,3)
		else if(length<=50)
			return "0010010" . this.IntToBinaryStringLSB(length-43,3)
		else if(length<=58)
			return "0010011" . this.IntToBinaryStringLSB(length-51,3)
		else if(length<=66)
			return "0010100" . this.IntToBinaryStringLSB(length-59,3)
		else if(length<=82)
			return "0010101" . this.IntToBinaryStringLSB(length-67,4)
		else if(length<=98)
			return "0010110" . this.IntToBinaryStringLSB(length-83,4)
		else if(length<=114)
			return "0010111" . this.IntToBinaryStringLSB(length-99,4)
		else if(length<=130)
			return "11000000" . this.IntToBinaryStringLSB(length-115,4)
		else if(length<=162)
			return "11000001" . this.IntToBinaryStringLSB(length-131,5)
		else if(length<=194)
			return "11000010" . this.IntToBinaryStringLSB(length-163,5)
		else if(length<=226)
			return "11000011" . this.IntToBinaryStringLSB(length-195,5)
		else if(length<=257)
			return "11000100" . this.IntToBinaryStringLSB(length-227,5)
		else if(length==258)
			return "11000101"
	}

	CalcDistanceCode(distance)
	{
		if(distance<=4)
			return this.IntToBinaryString(distance-1,5)
		else if(distance<=6)
			return "00100" . this.IntToBinaryStringLSB(distance-5,1)
		else if(distance<=8)
			return "00101" . this.IntToBinaryStringLSB(distance-7,1)
		else if(distance<=12)
			return "00110" . this.IntToBinaryStringLSB(distance-9,2)
		else if(distance<=16)
			return "00111" . this.IntToBinaryStringLSB(distance-13,2)
		else if(distance<=24)
			return "01000" . this.IntToBinaryStringLSB(distance-17,3)
		else if(distance<=32)
			return "01001" . this.IntToBinaryStringLSB(distance-25,3)
		else if(distance<=48)
			return "01010" . this.IntToBinaryStringLSB(distance-33,4)
		else if(distance<=64)
			return "01011" . this.IntToBinaryStringLSB(distance-49,4)
		else if(distance<=96)
			return "01100" . this.IntToBinaryStringLSB(distance-65,5)
		else if(distance<=128)
			return "01101" . this.IntToBinaryStringLSB(distance-97,5)
		else if(distance<=192)
			return "01110" . this.IntToBinaryStringLSB(distance-129,6)
		else if(distance<=256)
			return "01111" . this.IntToBinaryStringLSB(distance-193,6)
		else if(distance<=384)
			return "10000" . this.IntToBinaryStringLSB(distance-257,7)
		else if(distance<=512)
			return "10001" . this.IntToBinaryStringLSB(distance-385,7)
		else if(distance<=768)
			return "10010" . this.IntToBinaryStringLSB(distance-513,8)
		else if(distance<=1024)
			return "10011" . this.IntToBinaryStringLSB(distance-769,8)
		else if(distance<=1536)
			return "10100" . this.IntToBinaryStringLSB(distance-1025,9)
		else if(distance<=2048)
			return "10101" . this.IntToBinaryStringLSB(distance-1537,9)
		else if(distance<=3072)
			return "10110" . this.IntToBinaryStringLSB(distance-2049,10)
		else if(distance<=4096)
			return "10111" . this.IntToBinaryStringLSB(distance-3073,10)
		else if(distance<=6144)
			return "11000" . this.IntToBinaryStringLSB(distance-4097,11)
		else if(distance<=8192)
			return "11001" . this.IntToBinaryStringLSB(distance-6145,11)
		else if(distance<=12288)
			return "11010" . this.IntToBinaryStringLSB(distance-8193,12)
		else if(distance<=16384)
			return "11011" . this.IntToBinaryStringLSB(distance-12289,12)
		else if(distance<=24576)
			return "11100" . this.IntToBinaryStringLSB(distance-16385,13)
		else if(distance<=32768)
			return "11101" . this.IntToBinaryStringLSB(distance-24577,13)
	}

	IntToBinaryString(code,bits) ;Takes an Int and returns a binary string
	{
		Loop % bits
			bin:=(code >> (A_Index-1)) & 1 . bin
		return bin
	}

	IntToBinaryStringLSB(code,bits) ;Takes an Int and returns a binary string, LSB first
	{
		Loop % bits
			bin:=bin . (code >> (A_Index-1)) & 1
		return bin
	}

	BinaryStringToInt(bitString) ;Takes a binary string and returns an int, MSB at the start of the string
	{
		bits:=StrLen(bitString)
		int:=0
		loop, Parse, bitString
			int+=A_LoopField * 2**(bits-A_Index)
		return int
	}

	BinaryStringToIntLSB(bitString) ;Takes a binary string and returns an int, LSB at the start of the string
	{
		int:=0
		loop, Parse, bitString
			int+=A_LoopField * 2**(A_Index-1)
		return int
	}

	class IC_BrivMaster_Budget_Zlib_Read_Bitstream_Class ;TODO: Could we do something like this to assemble the string in the appropriate order when deflating? I.e. fill a byte buffer LSB first, and then add that byte once full. Maybe not worth the effort over doing something properly in binary
	{
		__new(binaryString)
		{
			this.BinaryString:=binaryString
			this.BitIndex:=0 ;Bit we are currently on within the byte, 0 = LSB, 7-MSB
			this.ByteIndex:=0 ;Byte we are currently on
		}

		ReadAsMSB(numBits) ;Reads the given number of bits, starting from the LSB of each byte, forming an MSB-based value, e.g. the hoffman symbols in a stream
		{
			bits:=""
			loop %numBits%
				bits.=this.NextBit()
			return bits
		}

		ReadAsLSB(numBits) ;Reads the given number of bits, starting from the LSB of each byte, forming an LSB-based value, e.g. the BTYPE value
		{
			bits:=""
			loop %numBits%
				bits:=this.NextBit() . bits
			return bits
		}

		NextBit()
		{
			bit:=SubStr(this.BinaryString,this.ByteIndex*8+(8-this.BitIndex),1)
			if(this.BitIndex==7)
			{
				this.BitIndex:=0
				this.ByteIndex++
			}
			else
				this.BitIndex++
			return bit
		}

		MoveToNextByte() ;Moves to the start of the next byte
		{
			if(this.BitIndex) ;If bitIndex isn't already 0 (first bit), move forward to the start of the next byte
			{
				this.BitIndex:=0
				this.ByteIndex++
			}
		}
	}
}