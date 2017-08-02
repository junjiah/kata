function getTotalFromCoins(coins) {
	const reduceFn = (acc, curr) => acc + Number(curr) * coins[curr];
	return Object.keys(coins).reduce(reduceFn, 0);
}

function VendingMachine(coins) {
	this.coins = coins;
}

VendingMachine.prototype.vending = function(price, inserted) {
	// Only keep valid coins. Also record invalid coins.
	const invalidCoins = {};
	const validCoins = {};
	for (const c of Object.keys(inserted)) {
		(this.coins[c] === undefined ? invalidCoins : validCoins)[c] = inserted[c];
	}

	const insertedPrice = getTotalFromCoins(validCoins);
	if (insertedPrice < price) {
		// Return whatever is inserted.
		return inserted;
	}

	// Merge valid coins with existing ones, and represent as a sorted list.
	Object.keys(validCoins).forEach(c => this.coins[c] += validCoins[c]);
	const coins = Object.keys(this.coins)
		.map(Number)
		.sort((a, b) => b - a)
		.map(c => [c, this.coins[c]]);

	const exchange = insertedPrice - price;

	// Dynamic programming. An array mapping price to required coin combination.
	const coinCombos = [];
	for (const [coin, number] of coins) {
		if (number > 0) {
			coinCombos[coin] = {[coin]: 1};
		}
	}
	const minCoin = coins[coins.length - 1][0];
	for (let i = minCoin; i <= exchange; ++i) {
		const combo = coinCombos[i];
		if (combo) {
			continue;
		}

		// Loop through available coins.
		for (const [c, n] of coins) {
			const prevCombo = coinCombos[i - c];
			if (!prevCombo) {
				// Can't reach that combination.
				continue;
			}
			const used = prevCombo[c] || 0;
			if (n <= used) {
				// Coin used up.
				continue;
			}
			coinCombos[i] = Object.assign({}, prevCombo, {[c]: used + 1});
			break;
		}
	}

	// Return the combo near `exchange`.
	let toReturn = {};
	for (let i = exchange; i >= minCoin; --i) {
		if (coinCombos[i]) {
			toReturn = coinCombos[i];
			// Substract coins from existing collection.
			Object.keys(toReturn).forEach(c => this.coins[c] -= toReturn[c]);
			break;
		}
	}
	// Add invalid coins to return.
	return Object.assign(toReturn, invalidCoins);
}


// Test cases.
const assert = require('chai').assert;

let vm = new VendingMachine({1: 1, 2: 0, 4: 3, 6: 2});
assert.deepEqual(vm.vending(12, {1: 3, 4: 2}), {1: 3, 4: 2}, 'insufficient balance');
assert.deepEqual(vm.vending(12, {1: 1, 5: 2}), {1: 1, 5: 2}, 'insufficient balance (in the machine there is no 5 coin value)');
assert.deepEqual(vm.vending(12, {4: 3}), {}, 'Exact price');
assert.deepEqual(vm.vending(12, {6: 1, 2: 3}), {}, 'Exact price');
assert.deepEqual(vm.vending(12, {6: 2}), {}, 'Exact price');
assert.deepEqual(vm.vending(12, {6: 1, 4: 2}), {2: 1});
assert.deepEqual(vm.vending(12, {6: 4}), {6: 2});
assert.deepEqual(vm.vending(12, {6: 4, 3: 4}), {3: 4, 6: 2});
assert.deepEqual(vm.vending(12, {4: 5}), {2: 1, 6: 1});
assert.deepEqual(vm.vending(17, {4: 4, 2: 1}), {1: 1});

vm = new VendingMachine({1: 0, 3: 2, 5: 1, 8: 0});
assert.deepEqual(vm.vending(9, {8: 2}), {3: 2});
assert.deepEqual(vm.coins, {1: 0, 3: 0, 5: 1, 8: 2});

vm = new VendingMachine({20: 0, 9: 2, 5: 2})
assert.deepEqual(vm.vending(1, {20: 1}), {5: 2, 9: 1});
assert.deepEqual(vm.coins, {5: 0, 9: 1, 20: 1});

vm = new VendingMachine({20: 0, 9: 2, 5: 2, 1: 19})
assert.deepEqual(vm.vending(1, {20: 1}), {9: 2, 1: 1});
assert.deepEqual(vm.coins, {1: 18, 5: 2, 9: 0, 20: 1});

vm = new VendingMachine({20: 0, 9: 2, 5: 2, 1: 19});
assert.deepEqual(vm.vending(1, {20: 1, 9: 1}), {20: 1, 5: 1, 1: 3});
