fn fibonacci() {
	let a = 0;
	let b = 1;

	while (a < 1000) {
		let c = a;
		a = a + b;
		b = c;
		print(a);
	}
}

fibonacci();
