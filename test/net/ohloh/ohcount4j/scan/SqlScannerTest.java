package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.LANG_SQL;

import org.testng.annotations.Test;

public class SqlScannerTest extends BaseScannerTest {
	
	@Test
	public void basic() {
		assertLine(new SqlScanner(), new Line(LANG_SQL, BLANK),   "\n");
		assertLine(new SqlScanner(), new Line(LANG_SQL, BLANK),   "     \n");
		assertLine(new SqlScanner(), new Line(LANG_SQL, BLANK),   "\t\n");
		assertLine(new SqlScanner(), new Line(LANG_SQL, CODE),    "SELECT * FROM test\n");
		assertLine(new SqlScanner(), new Line(LANG_SQL, COMMENT), "/* Block Comment */\n");
		assertLine(new SqlScanner(), new Line(LANG_SQL, COMMENT), "-- Line comment\n");
		assertLine(new SqlScanner(), new Line(LANG_SQL, CODE),    "CREATE TABLE 'test'( // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new SqlScanner(), new Line(LANG_SQL, BLANK),   "     ");
		assertLine(new SqlScanner(), new Line(LANG_SQL, BLANK),   "\t");
		assertLine(new SqlScanner(), new Line(LANG_SQL, CODE),    "SELECT * FROM test");
		assertLine(new SqlScanner(), new Line(LANG_SQL, COMMENT), "/* Block Comment */");
		assertLine(new SqlScanner(), new Line(LANG_SQL, COMMENT), "-- Line comment");
		assertLine(new SqlScanner(), new Line(LANG_SQL, CODE),    "CREATE TABLE 'test'( // with comment");
	}

	@Test
	public void simpleTest() {
		String code
			= "/* Simple Sql Script Test\n"
			+ "\t\n"
			+ " multi-line comment */\n"
			+ "\n"
			+ "CREATE TABLE test (\n"
			+ "\ttest_id int(10))\n"
			+ "\n"
			+ "SELECT * FROM test -- comment on code line\n";

		Line[] expected = {
			new Line(LANG_SQL, COMMENT),
			new Line(LANG_SQL, BLANK),
			new Line(LANG_SQL, COMMENT),
			new Line(LANG_SQL, BLANK),
			new Line(LANG_SQL, CODE),
			new Line(LANG_SQL, CODE),
			new Line(LANG_SQL, BLANK),
			new Line(LANG_SQL, CODE)
		};
		assertLines(new SqlScanner(), expected, code);
	}

	@Test
	public void unterminatedMultilineStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "'\nA\n\n";

		Line[] expected = {
				new Line(LANG_SQL, CODE),
				new Line(LANG_SQL, CODE),
				new Line(LANG_SQL, BLANK)
			};
		assertLines(new SqlScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "/*\n\n\n";

		Line[] expected = {
				new Line(LANG_SQL, COMMENT),
				new Line(LANG_SQL, BLANK),
				new Line(LANG_SQL, BLANK)
			};
		assertLines(new SqlScanner(), expected, code);
	}

}
