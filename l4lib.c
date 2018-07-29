int div(n, d) {
	if (d == 0 || (d == -1 && n == -2147483648)) {
		raise(8);
	}
	return n/d;
}

int mod(n, d) {
	if (d == 0 || (d == -1 && n == -2147483648)) {
		raise(8);
	}
	return n%d;
}

int sal(n, d) {
	if ((d >= 32) || (d < 0)) {
		raise(8);
	}
	return n << d;
}

int sar(n, d) {
	if ((d >= 32) || (d < 0)) {
		raise(8);
	}
	return n >> d;
}

