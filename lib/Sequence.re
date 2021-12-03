module type Reader = {
  type t('a);
  let split_head: t('a) => result(('a, t('a)), unit);
};
