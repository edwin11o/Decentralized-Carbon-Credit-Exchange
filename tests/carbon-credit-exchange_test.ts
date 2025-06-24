import {
  Clarinet,
  Tx,
  Chain,
  Account,
  types
} from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
  name: "Can add authorized verifier",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const verifier = accounts.get('wallet_1')!;
    
    let block = chain.mineBlock([
      Tx.contractCall('carbon-credit-exchange', 'add-verifier', [
        types.principal(verifier.address)
      ], deployer.address)
    ]);
    
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result, '(ok true)');
  },
});

Clarinet.test({
  name: "Can issue carbon credits by authorized verifier",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const verifier = accounts.get('wallet_1')!;
    const recipient = accounts.get('wallet_2')!;
    
    // First add verifier
    let block = chain.mineBlock([
      Tx.contractCall('carbon-credit-exchange', 'add-verifier', [
        types.principal(verifier.address)
      ], deployer.address)
    ]);
    
    // Then issue credits
    block = chain.mineBlock([
      Tx.contractCall('carbon-credit-exchange', 'issue-carbon-credits', [
        types.ascii("Solar Farm Project"),
        types.ascii("California, USA"),
        types.uint(1000),
        types.uint(2023),
        types.ascii("VCS"),
        types.principal(recipient.address),
        types.none()
      ], verifier.address)
    ]);
    
    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result, '(ok u1)');
  },
});
