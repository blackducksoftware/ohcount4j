package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class ErlangScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, BLANK),   "\n");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, BLANK),   "     \n");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, BLANK),   "\t\n");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, CODE),    "import Erlang.util.List;\n");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, COMMENT), "%% Line comment\n");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, COMMENT), "%\n");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, CODE),    "import Erlang.util.List; % with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, BLANK),   "     ");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, BLANK),   "\t");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, CODE),    "import Erlang.util.List;");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, COMMENT), "%% Line comment");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, COMMENT), "%");
		assertLine(new ErlangScanner(), new Line(Language.ERLANG, CODE),    "import Erlang.util.List; % with comment");
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
			new Line(Language.ERLANG, COMMENT),
			new Line(Language.ERLANG, COMMENT),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, BLANK),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, BLANK),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE),
			new Line(Language.ERLANG, CODE)
		};
		assertLines(new ErlangScanner(), expected, code);
	}

}
