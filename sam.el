;; Internal boxes only reachable via two hops.
;;
;; internet --> caketown (office) --> util1 (dc bastion host) --> box
;;

(setq internal-hosts '("rebound01"
                       "sandbox01"
                       "tsaa01"
                       "tsaa12"
                       "caumng01"
                       "cauapp21"
                       "cauapp22"
                       "cauapp23"
                       "cauapp24"
                       "cauapp25"
                       "cauapp26"
                       "cauapp27"
                       "cauapp28"
                       "cauapp29"
                       "cauapp30"
                       "cauapp31"
                       "cauapp32"
                       "cauapp33"
                       "cauapp34"
                       "cauapp35"
                       "cauapp36"
                       "cauapp37"
                       "cauapp38"
                       "cauapp39"
                       "cauapp40"
                       "worker01"
                       "worker02"
                       "worker03"
                       "tsaa69"
                       "partner01"
                       "partner02"
                       "wshapp01"
                       "wshapp02"
                       "wshapp03"
                       "wshapp04"
                       "wshapp05"
                       "wshapp06"
                       "wishes07"
                       "wishes08"
                       "wishes09"
                       "wishes10"
                       "blog01"
                       "pmta01"
                       "pmta03"))

(mapcar (lambda (host)
          (add-to-list 'tramp-default-proxies-alist
                       (list host nil "/ssh:root@util1:")))
        internal-hosts)

(add-to-list 'tramp-default-proxies-alist
             '("util1" nil "/ssh:caketown.causes.com:"))

;; can't ssh to root@devbox directly; have to sudo
(add-to-list 'tramp-default-proxies-alist
             '("devbox" "root" "/ssh:%h:"))

(color-theme-twilight)
