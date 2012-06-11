package org.eclipse.cdt.core.parser.tests.scanner;

import junit.framework.TestSuite;

import org.eclipse.cdt.core.testplugin.util.BaseTestCase;
import org.eclipse.cdt.internal.core.parser.scanner.DefaultCharGetter;
import org.eclipse.cdt.internal.core.parser.scanner.NumberParser;
import org.eclipse.cdt.internal.core.parser.scanner.Lexer.LexerOptions;
import org.eclipse.cdt.internal.core.parser.scanner.NumberToken;

public class NumberParserTests extends BaseTestCase {
	
	DefaultCharGetter cg;
	NumberParser np;
	NumberToken tok;
	int fStart;
	
	public static TestSuite suite() {
		return suite(NumberParserTests.class);
	}
	
	private void init(String image) {
		init(image, 0);
	}
	
	private void init(String image, int offset) {
		cg = new DefaultCharGetter(image.toCharArray(), offset);
		fStart = offset;
	}
	
	private void initNP(String image) {
		np = new NumberParser(new LexerOptions(), "ulUL".toCharArray(), new DefaultCharGetter(image.toCharArray(), 0));
		tok = np.getNumber();
	}
	
	private void validateOffset(int offset) {
		assertEquals(offset, cg.getOffset());
	}
	
	private void validateChar(int c) {
		assertEquals(c, cg.get());
	}
	
	private void validateNext(int c) {
		assertEquals(c, cg.next());
	}
	
	private void validateSubstring(String s) {
		char[] result = cg.getSubstring(fStart, cg.getOffset() - fStart - 1);
		assertEquals(s.length(), result.length);
		
		for (int i = 0; i < result.length; i++) {
			assertEquals(s.charAt(i), result[i]);
		}
	}
	
	private void validateSuccessfulParse() {
		assertNotNull(tok);
		assertEquals(tok.hasFailed(), false);
	}
	
	private void validateImage(String expected) {
		String result = tok.getImage();
		
		assertEquals(result.length(), expected.length());
		assertTrue(expected.equals(result));
	}
	
	private void validateNumberImage(String expected) {
		String result = new String(tok.getNumber());
		
		assertEquals(result.length(), expected.length());
		assertTrue(expected.equals(result));
	}
	
	private void validateBinary(String expected) {
		assertNotNull(tok);
		assertEquals(tok.getType(), NumberToken.NumberType.BINARY);
		
		validateNumberImage(expected);
	}
	
	private void validateFloat(String expected) {
		assertNotNull(tok);
		assertEquals(tok.getType(), NumberToken.NumberType.FLOAT);
		
		validateNumberImage(expected);
	}
	
	private void validateDecimal(String expected) {
		assertNotNull(tok);
		assertEquals(tok.getType(), NumberToken.NumberType.DECIMAL);
		
		validateNumberImage(expected);
	}
	
	private void validateHex(String expected) {
		assertNotNull(tok);
		assertEquals(tok.getType(), NumberToken.NumberType.HEX);
		
		validateNumberImage(expected);
	}
	
	private void validateHexfloat(String expected) {
		assertNotNull(tok);
		assertEquals(tok.getType(), NumberToken.NumberType.HEXFLOAT);
		
		validateNumberImage(expected);
	}
	
	private void validateOctal(String expected) {
		assertNotNull(tok);
		assertEquals(tok.getType(), NumberToken.NumberType.OCTAL);
		
		validateNumberImage(expected);
	}
	
	private void validateSuffix(String expected) {
		assertNotNull(tok);
		String result = new String(tok.getSuffix());
		
		assertEquals(expected.length(), result.length());
		
		assertTrue(result.equals(expected));
	}
	
	public void testDefaultGetterSimple1() {
		init("1");
		validateOffset(0);
		validateChar('1');
		validateNext(-1);
		validateSubstring("1");
		validateOffset(1);
	}
	
	public void testDefaultGetterSimple2() {
		init("12");
		validateOffset(0);
		validateChar('1');
		validateOffset(0);
		validateNext('2');
		validateOffset(1);
		validateChar('2');
		validateOffset(2);
		validateNext(-1);
		validateOffset(2);
		validateSubstring("12");
	}
	
	public void testDefaultGetterSimple3() {
		init("a12", 1);
		validateOffset(1);
		validateChar('1');
		validateNext('2');
		validateOffset(2);
		validateNext(-1);
		validateOffset(3);
		validateSubstring("12");
	}
	
	public void testNumberParserEmpty() {
		initNP("");
		assertNull(tok);
	}
	
	public void testNumberParser1() {
		initNP("1");
		validateSuccessfulParse();
		validateDecimal("1");
		validateSuffix("");
	}
	
	public void testNumberParser2() {
		initNP("12");
		validateSuccessfulParse();
		validateDecimal("12");
		validateSuffix("");
	}
	
	public void testNumberParser3() {
		initNP("123U");
		validateSuccessfulParse();
		validateDecimal("123");
		validateSuffix("U");
	}
	
	public void testNumberParser4() {
		initNP("123L");
		validateSuccessfulParse();
		validateDecimal("123");
		validateSuffix("L");
	}

}
