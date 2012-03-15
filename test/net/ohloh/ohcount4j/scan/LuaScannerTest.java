package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import net.ohloh.ohcount4j.Language;

import org.testng.annotations.Test;

public class LuaScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new LuaScanner(), new Line(Language.LUA, BLANK),   "\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, BLANK),   "     \n");
		assertLine(new LuaScanner(), new Line(Language.LUA, BLANK),   "\t\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, CODE),    "function factorial(n)\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--[[ Block Comment ]]\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "-- Line comment\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, CODE),    "function factorial(n) -- with comment\n");
		/* 
		 * These test to ensure that improperly formatted block comments that have proper
		 * line comment start are still seen as line comments
		 */
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--[ [ ]]\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "-- [[ ]]\n");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--[\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new LuaScanner(), new Line(Language.LUA, BLANK),   "     ");
		assertLine(new LuaScanner(), new Line(Language.LUA, BLANK),   "\t");
		assertLine(new LuaScanner(), new Line(Language.LUA, CODE),    "function factorial(n)");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--[[ Block Comment ]]");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "-- Line comment");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--");
		assertLine(new LuaScanner(), new Line(Language.LUA, CODE),    "function factorial(n) -- with comment");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--[ [ ]]");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "-- [[ ]]");
		assertLine(new LuaScanner(), new Line(Language.LUA, COMMENT), "--[");
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
			new Line(Language.LUA, COMMENT),
			new Line(Language.LUA, BLANK),
			new Line(Language.LUA, COMMENT),
			new Line(Language.LUA, COMMENT),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, COMMENT),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, BLANK),
			new Line(Language.LUA, CODE),
			new Line(Language.LUA, COMMENT)
		};
		assertLines(new LuaScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "--[[\n\n\n";

		Line[] expected = {
				new Line(Language.LUA, COMMENT),
				new Line(Language.LUA, BLANK),
				new Line(Language.LUA, BLANK)
			};
		assertLines(new LuaScanner(), expected, code);
	}
	
}
