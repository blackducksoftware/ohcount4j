package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.LANG_LUA;

import org.testng.annotations.Test;

public class LuaScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new LuaScanner(), new Line(LANG_LUA, BLANK),   "\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, BLANK),   "     \n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, BLANK),   "\t\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, CODE),    "function factorial(n)\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "--[[ Block Comment ]]\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "-- Line comment\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "--\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, CODE),    "function factorial(n) -- with comment\n");
		/* 
		 * These test to ensure that improperly formatted block comments that have proper
		 * line comment start are still seen as line comments
		 */
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "--[ [ ]]\n");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "-- [[ ]]\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new LuaScanner(), new Line(LANG_LUA, BLANK),   "     ");
		assertLine(new LuaScanner(), new Line(LANG_LUA, BLANK),   "\t");
		assertLine(new LuaScanner(), new Line(LANG_LUA, CODE),    "function factorial(n)");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "--[[ Block Comment ]]");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "-- Line comment");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "--");
		assertLine(new LuaScanner(), new Line(LANG_LUA, CODE),    "function factorial(n) -- with comment");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "--[ [ ]]");
		assertLine(new LuaScanner(), new Line(LANG_LUA, COMMENT), "-- [[ ]]");
	}

	@Test
	public void sampleTest() {
		String code
			= "--[[ Test Lua Program\n"
			+ "\t\n"
			+ "Multi Line Comment ]]\n"
			+ "		-- Computes Factorial of n\n"
			+ "		function factorial(n)\n"
			+ "		--[ [ this improperly formatted block comment is line comment]]\n"
			+ "			if n == 0 then --[[ comment on code line ]]\n"
			+ "				return 1\n"
			+ "			else\n"
			+ "				return n * factorial(n - 1)\n"
			+ "		end\n"
			+ "\t\n"
			+ "end\n"
			+ "--";
			
		Line[] expected = {
			new Line(LANG_LUA, COMMENT),
			new Line(LANG_LUA, BLANK),
			new Line(LANG_LUA, COMMENT),
			new Line(LANG_LUA, COMMENT),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, COMMENT),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, BLANK),
			new Line(LANG_LUA, CODE),
			new Line(LANG_LUA, COMMENT)
		};
		assertLines(new LuaScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "--[[\n\n\n";

		Line[] expected = {
				new Line(LANG_LUA, COMMENT),
				new Line(LANG_LUA, BLANK),
				new Line(LANG_LUA, BLANK)
			};
		assertLines(new LuaScanner(), expected, code);
	}
	
}
