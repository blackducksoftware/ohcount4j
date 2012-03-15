package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

import org.testng.annotations.Test;

public class SqlScannerTest extends BaseScannerTest {
	
	@Test
	public void basic() {
		assertLine(new SqlScanner(), new Line(Language.SQL, BLANK),   "\n");
		assertLine(new SqlScanner(), new Line(Language.SQL, BLANK),   "     \n");
		assertLine(new SqlScanner(), new Line(Language.SQL, BLANK),   "\t\n");
		assertLine(new SqlScanner(), new Line(Language.SQL, CODE),    "SELECT * FROM test\n");
		assertLine(new SqlScanner(), new Line(Language.SQL, COMMENT), "/* Block Comment */\n");
		assertLine(new SqlScanner(), new Line(Language.SQL, COMMENT), "-- Line comment\n");
		assertLine(new SqlScanner(), new Line(Language.SQL, COMMENT), "--\n");
		assertLine(new SqlScanner(), new Line(Language.SQL, CODE),    "CREATE TABLE 'test'( // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new SqlScanner(), new Line(Language.SQL, BLANK),   "     ");
		assertLine(new SqlScanner(), new Line(Language.SQL, BLANK),   "\t");
		assertLine(new SqlScanner(), new Line(Language.SQL, CODE),    "SELECT * FROM test");
		assertLine(new SqlScanner(), new Line(Language.SQL, COMMENT), "/* Block Comment */");
		assertLine(new SqlScanner(), new Line(Language.SQL, COMMENT), "-- Line comment");
		assertLine(new SqlScanner(), new Line(Language.SQL, COMMENT), "--");
		assertLine(new SqlScanner(), new Line(Language.SQL, CODE),    "CREATE TABLE 'test'( // with comment");
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
			new Line(Language.SQL, COMMENT),
			new Line(Language.SQL, BLANK),
			new Line(Language.SQL, COMMENT),
			new Line(Language.SQL, BLANK),
			new Line(Language.SQL, CODE),
			new Line(Language.SQL, CODE),
			new Line(Language.SQL, BLANK),
			new Line(Language.SQL, CODE)
		};
		assertLines(new SqlScanner(), expected, code);
	}

	@Test
	public void unterminatedSqlBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "{\n\n\n";

		Line[] expected = {
				new Line(Language.SQL, COMMENT),
				new Line(Language.SQL, BLANK),
				new Line(Language.SQL, BLANK)
		};
		assertLines(new SqlScanner(), expected, code);
	}
	
}
