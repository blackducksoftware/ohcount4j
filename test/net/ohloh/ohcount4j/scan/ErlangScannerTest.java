package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_ERLANG;

public class ErlangScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, BLANK),   "\n");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, BLANK),   "     \n");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, BLANK),   "\t\n");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, CODE),    "import Erlang.util.List;\n");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, COMMENT), "%% Line comment\n");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, COMMENT), "%\n");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, CODE),    "import Erlang.util.List; % with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, BLANK),   "     ");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, BLANK),   "\t");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, CODE),    "import Erlang.util.List;");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, COMMENT), "%% Line comment");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, COMMENT), "%");
		assertLine(new ErlangScanner(), new Line(LANG_ERLANG, CODE),    "import Erlang.util.List; % with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "%% A type of hello world server request program\n"
			+ "%% Written in Erlang\n"
			+ "-module(hello).\n"
			+ "-export([start/0]).\n"
			+ "\n"
			+ "start() ->\n"
			+ "		spawn(fun() -> loop() end).\n"
			+ "loop() ->\n"
			+ "		receive % When receiving request print Hello, World!~\n"
			+ "			hello ->\n"
			+ "				io:format(\"Hello, World!~n\"),\n"
			+ "				loop();\n"
			+ "\t\n"
			+ "			goodbye ->\n"
			+ "				ok\n"
			+ "		end.\n";

		Line[] expected = {
			new Line(LANG_ERLANG, COMMENT),
			new Line(LANG_ERLANG, COMMENT),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, BLANK),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, BLANK),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE),
			new Line(LANG_ERLANG, CODE)
		};
		assertLines(new ErlangScanner(), expected, code);
	}

}