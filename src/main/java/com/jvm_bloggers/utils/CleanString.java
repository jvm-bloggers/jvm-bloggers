/*
 * Copyright (c) 2020. BEST S.A. and/or its affiliates. All rights reserved.
 */
package com.jvm_bloggers.utils;

import java.math.BigDecimal;
import java.math.RoundingMode;

public class CleanString {
	private static final BigDecimal MAX_UPPER_CASE_PERCENT = new BigDecimal("0.35");

	public static String clean(String input) {
		if (isEmpty(input)) {
			return input;
		}
		if (hasMoreUpperCaseThanAcceptableLimit(input)) {
			return lowerCaseAllButFirstChar(input);
		}
		return input;
	}

	private static boolean isEmpty(String input) {
		return input == null || input.isEmpty();
	}

	private static boolean hasMoreUpperCaseThanAcceptableLimit(String input) {
		return upperCasePercent(input).compareTo(MAX_UPPER_CASE_PERCENT) > 0;
	}

	private static BigDecimal upperCasePercent(String input) {
		int totalChars = input.length();
		long upperCaseLetters = countUpperCaseLetters(input);
		return divide(upperCaseLetters, totalChars);
	}

	private static long countUpperCaseLetters(String input) {
		return input.chars().filter(Character::isUpperCase).count();
	}

	private static BigDecimal divide(long upperCaseLetters, long totalChars) {
		if(totalChars == 0 || upperCaseLetters == 0) {
			return BigDecimal.ZERO;
		}
		return new BigDecimal(upperCaseLetters).divide(new BigDecimal(totalChars), 2, RoundingMode.HALF_UP);
	}

	private static String lowerCaseAllButFirstChar(String input) {
		return input.substring(0,1) + input.substring(1).toLowerCase();
	}
}
